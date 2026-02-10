import { useState, useEffect, useCallback } from "react";
import { api, Message, WSEvent } from "../api/client";

// Counter for unique WebSocket message IDs (negative to avoid collision with server IDs)
let wsMessageIdCounter = -1;

export function useMessages(sessionId: string | null) {
    const [messages, setMessages] = useState<Message[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [sending, setSending] = useState(false);

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
            setError(null);
        } catch (e) {
            setError(
                e instanceof Error ? e.message : "Failed to fetch messages",
            );
        } finally {
            setLoading(false);
        }
    }, [sessionId]);

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
        error,
        sending,
        sendMessage,
        refetch: fetchMessages,
    };
}
