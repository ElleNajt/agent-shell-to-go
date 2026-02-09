import { useState, useEffect, useCallback } from "react";
import AsyncStorage from "@react-native-async-storage/async-storage";

const STORAGE_KEY = "agent_last_viewed";

interface LastViewedMap {
    [sessionId: string]: string; // ISO timestamp
}

export function useUnreadAgents() {
    const [lastViewed, setLastViewed] = useState<LastViewedMap>({});

    useEffect(() => {
        loadLastViewed();
    }, []);

    const loadLastViewed = async () => {
        try {
            const stored = await AsyncStorage.getItem(STORAGE_KEY);
            if (stored) {
                setLastViewed(JSON.parse(stored));
            }
        } catch (e) {
            console.error("Failed to load last viewed:", e);
        }
    };

    const markAsViewed = useCallback(
        async (sessionId: string) => {
            const now = new Date().toISOString();
            const updated = { ...lastViewed, [sessionId]: now };
            setLastViewed(updated);
            try {
                await AsyncStorage.setItem(
                    STORAGE_KEY,
                    JSON.stringify(updated),
                );
            } catch (e) {
                console.error("Failed to save last viewed:", e);
            }
        },
        [lastViewed],
    );

    const hasUnread = useCallback(
        (
            sessionId: string,
            lastActivity: string,
            lastMessageRole: string,
        ): boolean => {
            // Only count as unread if last message was from agent (not user)
            if (lastMessageRole === "user") return false;

            const viewed = lastViewed[sessionId];
            // If never viewed in the app, don't mark as unread
            // (user may have seen it in Emacs or just opened the app)
            if (!viewed) return false;

            // Only unread if there's new activity since we last viewed
            return new Date(lastActivity) > new Date(viewed);
        },
        [lastViewed],
    );

    return { hasUnread, markAsViewed };
}
