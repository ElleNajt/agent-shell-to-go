import { useState, useEffect, useCallback, useRef } from "react";
import { api, Message, WSEvent } from "../api/client";

// Counter for unique WebSocket message IDs (negative to avoid collision with server IDs)
let wsMessageIdCounter = -1;

export function useMessages(sessionId: string | null) {
    const [messages, setMessages] = useState<Message[]>([]);
    const [loading, setLoading] = useState(false);
    const [loadingMore, setLoadingMore] = useState(false);
    const [hasMore, setHasMore] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [sending, setSending] = useState(false);
    const oldestTimestamp = useRef<string | null>(null);

    const fetchMessages = useCallback(async () => {
        if (!sessionId) return;

        setLoading(true);
        try {
            const data = await api.getMessages(sessionId);
            // Deduplicate by id (in case DB has duplicates)
            const seen = new Set<number>();
            const deduped = data.filter((m: Message) => {
                if (seen.has(m.id)) return false;
                seen.add(m.id);
                return true;
            });
            setMessages(deduped);
            // Track oldest timestamp for pagination
            if (deduped.length > 0) {
                oldestTimestamp.current = deduped[0].timestamp;
            }
            // If we got fewer than 50 messages, there's no more to load
            setHasMore(deduped.length >= 50);
            setError(null);
        } catch (e) {
            setError(
                e instanceof Error ? e.message : "Failed to fetch messages",
            );
        } finally {
            setLoading(false);
        }
    }, [sessionId]);

    const loadMoreMessages = useCallback(async () => {
        if (!sessionId || loadingMore || !hasMore || !oldestTimestamp.current)
            return;

        setLoadingMore(true);
        try {
            const olderMessages = await api.getMessages(
                sessionId,
                50,
                oldestTimestamp.current,
            );
            if (olderMessages.length === 0) {
                setHasMore(false);
                return;
            }
            // Deduplicate
            const seen = new Set<number>();
            const deduped = olderMessages.filter((m: Message) => {
                if (seen.has(m.id)) return false;
                seen.add(m.id);
                return true;
            });
            // Update oldest timestamp
            if (deduped.length > 0) {
                oldestTimestamp.current = deduped[0].timestamp;
            }
            // Prepend older messages
            setMessages((prev) => {
                const existingIds = new Set(prev.map((m) => m.id));
                const newMessages = deduped.filter(
                    (m) => !existingIds.has(m.id),
                );
                return [...newMessages, ...prev];
            });
            // If we got fewer than 50, no more to load
            setHasMore(deduped.length >= 50);
        } catch (e) {
            setError(
                e instanceof Error ? e.message : "Failed to load more messages",
            );
        } finally {
            setLoadingMore(false);
        }
    }, [sessionId, loadingMore, hasMore]);

    useEffect(() => {
        if (!sessionId || !api.isConfigured()) return;

        fetchMessages();

        // Subscribe to WebSocket for real-time updates
        const unsubscribe = api.subscribe((event: WSEvent) => {
            if (
                event.type === "message" &&
                event.payload.session_id === sessionId
            ) {
                // Skip user messages - we already add them optimistically when sending
                if (event.payload.role === "user") return;

                setMessages((prev) => {
                    const newMsg = {
                        id: wsMessageIdCounter--,
                        session_id: event.payload.session_id,
                        role: event.payload.role as "user" | "agent" | "tool",
                        content: event.payload.content,
                        timestamp: event.payload.timestamp,
                    };
                    return [...prev, newMsg];
                });
            }

            // Handle status events - show system messages for important status changes
            if (
                event.type === "status" &&
                event.payload.session_id === sessionId &&
                event.payload.status === "interrupted"
            ) {
                setMessages((prev) => {
                    const systemMsg = {
                        id: wsMessageIdCounter--,
                        session_id: event.payload.session_id,
                        role: "system" as "user" | "agent" | "tool",
                        content: "--- Interrupted ---",
                        timestamp: event.payload.timestamp,
                    };
                    return [...prev, systemMsg];
                });
            }
        });

        return () => {
            unsubscribe();
        };
    }, [sessionId, fetchMessages]);

    const sendMessage = useCallback(
        async (content: string) => {
            if (!sessionId || !content.trim()) return;

            // Optimistically add the message with 'sending' status
            const optimisticMessage: Message = {
                id: wsMessageIdCounter--,
                session_id: sessionId,
                role: "user",
                content: content.trim(),
                timestamp: new Date().toISOString(),
                status: "sending",
            };
            setMessages((prev) => [...prev, optimisticMessage]);

            setSending(true);
            try {
                await api.sendMessage(sessionId, content);
                // Mark as 'sent' - server received and stored it
                setMessages((prev) =>
                    prev.map((m) =>
                        m.id === optimisticMessage.id
                            ? { ...m, status: "sent" as const }
                            : m,
                    ),
                );
            } catch (e) {
                // Remove optimistic message on failure
                setMessages((prev) =>
                    prev.filter((m) => m.id !== optimisticMessage.id),
                );
                setError(
                    e instanceof Error ? e.message : "Failed to send message",
                );
            } finally {
                setSending(false);
            }
        },
        [sessionId],
    );

    return {
        messages,
        loading,
        loadingMore,
        hasMore,
        error,
        sending,
        sendMessage,
        loadMore: loadMoreMessages,
        refetch: fetchMessages,
    };
}
