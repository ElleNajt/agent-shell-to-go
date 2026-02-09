import { useState, useEffect, useCallback, useRef } from 'react';
import { api, Message, WSEvent } from '../api/client';

export function useMessages(sessionId: string | null) {
  const [messages, setMessages] = useState<Message[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [sending, setSending] = useState(false);
  
  // Track message count to detect new messages via polling
  const lastMessageCount = useRef(0);
  const pollInterval = useRef<ReturnType<typeof setInterval> | null>(null);

  const fetchMessages = useCallback(async (silent = false) => {
    if (!sessionId) return;
    
    if (!silent) setLoading(true);
    try {
      const data = await api.getMessages(sessionId);
      
      // Only update if we have new messages (avoids flicker)
      if (data.length !== lastMessageCount.current) {
        lastMessageCount.current = data.length;
        setMessages(data);
      }
      setError(null);
    } catch (e) {
      if (!silent) {
        setError(e instanceof Error ? e.message : 'Failed to fetch messages');
      }
    } finally {
      if (!silent) setLoading(false);
    }
  }, [sessionId]);

  useEffect(() => {
    if (!sessionId || !api.isConfigured()) return;
    
    fetchMessages();

    // Subscribe to WebSocket for real-time updates
    const unsubscribe = api.subscribe((event: WSEvent) => {
      if (event.type === 'message' && event.payload.session_id === sessionId) {
        // Skip user messages - we already add them optimistically when sending
        if (event.payload.role === 'user') return;
        
        setMessages(prev => {
          const newMsg = {
            id: Date.now(),
            session_id: event.payload.session_id,
            role: event.payload.role as 'user' | 'agent' | 'tool',
            content: event.payload.content,
            timestamp: event.payload.timestamp,
          };
          lastMessageCount.current = prev.length + 1;
          return [...prev, newMsg];
        });
      }
    });

    // Polling fallback: check for new messages every 3 seconds
    // This handles cases where WebSocket is flaky
    pollInterval.current = setInterval(() => {
      fetchMessages(true); // silent fetch
    }, 3000);

    return () => {
      unsubscribe();
      if (pollInterval.current) {
        clearInterval(pollInterval.current);
      }
    };
  }, [sessionId, fetchMessages]);

  const sendMessage = useCallback(async (content: string) => {
    if (!sessionId || !content.trim()) return;

    // Optimistically add the message with 'sending' status
    const optimisticMessage: Message = {
      id: Date.now(),
      session_id: sessionId,
      role: 'user',
      content: content.trim(),
      timestamp: new Date().toISOString(),
      status: 'sending',
    };
    setMessages(prev => {
      lastMessageCount.current = prev.length + 1;
      return [...prev, optimisticMessage];
    });

    setSending(true);
    try {
      await api.sendMessage(sessionId, content);
      // Mark as 'sent' - server received and stored it
      setMessages(prev => prev.map(m => 
        m.id === optimisticMessage.id ? { ...m, status: 'sent' as const } : m
      ));
    } catch (e) {
      // Remove optimistic message on failure
      setMessages(prev => {
        lastMessageCount.current = prev.length - 1;
        return prev.filter(m => m.id !== optimisticMessage.id);
      });
      setError(e instanceof Error ? e.message : 'Failed to send message');
    } finally {
      setSending(false);
    }
  }, [sessionId]);

  return { messages, loading, error, sending, sendMessage, refetch: fetchMessages };
}
