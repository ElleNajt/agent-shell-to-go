import React, { useState, useEffect } from "react";
import { StatusBar } from "expo-status-bar";
import { StyleSheet, View } from "react-native";
import { SafeAreaProvider, SafeAreaView } from "react-native-safe-area-context";
import AsyncStorage from "@react-native-async-storage/async-storage";

import { api } from "./src/api/client";
import { Agent } from "./src/api/client";
import { SettingsScreen } from "./src/screens/SettingsScreen";
import { AgentsScreen } from "./src/screens/AgentsScreen";
import { ChatScreen } from "./src/screens/ChatScreen";

type Screen = "settings" | "agents" | "chat";

// Try to load config from config.json at build time
interface Machine {
    name: string;
    url: string;
}
let buildTimeConfig: { machines?: Machine[]; backendUrl?: string } = {};
try {
    buildTimeConfig = require("./config.json");
} catch (e) {
    // No config file, will use AsyncStorage
}

export default function App() {
    const [screen, setScreen] = useState<Screen>("settings");
    const [selectedAgent, setSelectedAgent] = useState<Agent | null>(null);
    const [loading, setLoading] = useState(true);
    const [graphViewport, setGraphViewport] = useState({
        offset: { x: 0, y: 0 },
        scale: 1,
    });

    useEffect(() => {
        checkExistingConfig();
    }, []);

    const checkExistingConfig = async () => {
        try {
            // First check build-time config for machines
            if (
                buildTimeConfig.machines &&
                buildTimeConfig.machines.length > 0
            ) {
                // Save machines to AsyncStorage so AgentsScreen can use them
                await AsyncStorage.setItem(
                    "agent_shell_machines",
                    JSON.stringify(buildTimeConfig.machines),
                );
                // Use first machine as default, or saved selection
                const savedUrl = await AsyncStorage.getItem(
                    "agent_shell_current_machine_url",
                );
                const machine = savedUrl
                    ? buildTimeConfig.machines.find(
                          (m) => m.url === savedUrl,
                      ) || buildTimeConfig.machines[0]
                    : buildTimeConfig.machines[0];
                api.configure(machine.url);
                api.connectWebSocket();
                setScreen("agents");
                setLoading(false);
                return;
            }

            // Legacy: single backendUrl
            if (buildTimeConfig.backendUrl) {
                api.configure(buildTimeConfig.backendUrl);
                api.connectWebSocket();
                setScreen("agents");
                setLoading(false);
                return;
            }

            // Fall back to AsyncStorage
            const url = await AsyncStorage.getItem("agent_shell_backend_url");

            if (url) {
                api.configure(url);
                api.connectWebSocket();
                setScreen("agents");
            }
        } catch (e) {
            console.error("Failed to check config:", e);
        } finally {
            setLoading(false);
        }
    };

    const handleConfigured = () => {
        setScreen("agents");
    };

    const handleSelectAgent = (agent: Agent) => {
        setSelectedAgent(agent);
        setScreen("chat");
    };

    const handleBackFromChat = () => {
        setScreen("agents");
        setSelectedAgent(null);
    };

    if (loading) {
        return null; // Or a splash screen
    }

    return (
        <SafeAreaProvider>
            <SafeAreaView
                style={styles.container}
                edges={["top", "left", "right"]}
            >
                <StatusBar style="light" />

                {screen === "settings" && (
                    <SettingsScreen onConfigured={handleConfigured} />
                )}

                {screen === "agents" && (
                    <AgentsScreen
                        selectedAgent={selectedAgent}
                        onSelectAgent={handleSelectAgent}
                        graphViewport={graphViewport}
                        onGraphViewportChange={setGraphViewport}
                    />
                )}

                {screen === "chat" && selectedAgent && (
                    <ChatScreen
                        agent={selectedAgent}
                        onBack={handleBackFromChat}
                    />
                )}
            </SafeAreaView>
        </SafeAreaProvider>
    );
}

const styles = StyleSheet.create({
    container: {
        flex: 1,
        backgroundColor: "#121212",
    },
});
