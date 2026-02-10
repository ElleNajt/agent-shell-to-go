import React, { useState, useRef, useEffect } from "react";
import {
    View,
    Text,
    FlatList,
    TextInput,
    TouchableOpacity,
    StyleSheet,
    KeyboardAvoidingView,
    Platform,
    ActivityIndicator,
    Alert,
    Modal,
    Pressable,
} from "react-native";
import * as Clipboard from "expo-clipboard";
import Markdown from "react-native-markdown-display";
import { useMessages } from "../hooks/useMessages";
import { Agent, Message, api } from "../api/client";
import { FileExplorerScreen } from "./FileExplorerScreen";

interface ChatScreenProps {
    agent: Agent;
    onBack: () => void;
}

export function ChatScreen({ agent, onBack }: ChatScreenProps) {
    const { messages, loading, error, sending, sendMessage } = useMessages(
        agent.session_id,
    );
    const [inputText, setInputText] = useState("");
    const [expandedTools, setExpandedTools] = useState<Set<number>>(new Set());
    const [showFileExplorer, setShowFileExplorer] = useState(false);
    const flatListRef = useRef<FlatList>(null);
    const [showScrollButton, setShowScrollButton] = useState(false);
    const scrollMetrics = useRef({ distanceFromBottom: 0, isAtBottom: true });

    const scrollToBottom = () => {
        flatListRef.current?.scrollToEnd({ animated: true });
        // Hide button immediately when user taps it
        setShowScrollButton(false);
        scrollMetrics.current.isAtBottom = true;
    };

    const handleScroll = (event: any) => {
        const { layoutMeasurement, contentOffset, contentSize } =
            event.nativeEvent;
        const distanceFromBottom =
            contentSize.height - layoutMeasurement.height - contentOffset.y;
        scrollMetrics.current.distanceFromBottom = distanceFromBottom;
        scrollMetrics.current.isAtBottom = distanceFromBottom <= 100;
        setShowScrollButton(distanceFromBottom > 100);
    };

    // Check if we should show scroll button when content changes
    const handleContentSizeChange = (width: number, height: number) => {
        // If user was at bottom, stay at bottom
        if (scrollMetrics.current.isAtBottom) {
            flatListRef.current?.scrollToEnd({ animated: false });
            setShowScrollButton(false);
        } else {
            // User was scrolled up, show the button
            setShowScrollButton(true);
        }
    };

    const handleSend = () => {
        if (inputText.trim() && !sending) {
            sendMessage(inputText.trim());
            setInputText("");
        }
    };

    const handleStop = async () => {
        try {
            await api.stopAgent(agent.session_id);
        } catch (e) {
            Alert.alert("Error", "Failed to stop agent");
        }
    };

    const handleClose = () => {
        Alert.alert(
            "Close Agent",
            `Are you sure you want to close "${agent.buffer_name.split(" @ ")[0]}"?`,
            [
                { text: "Cancel", style: "cancel" },
                {
                    text: "Close",
                    style: "destructive",
                    onPress: async () => {
                        try {
                            await api.closeAgent(agent.session_id);
                            onBack();
                        } catch (e) {
                            Alert.alert("Error", "Failed to close agent");
                        }
                    },
                },
            ],
        );
    };

    const handleRestart = () => {
        // TODO: Use proper acp/agent-shell resume functionality when available
        Alert.alert(
            "Restart Agent",
            `This will restart "${agent.buffer_name.split(" @ ")[0]}" with context from the last 10 messages. Continue?`,
            [
                { text: "Cancel", style: "cancel" },
                {
                    text: "Restart",
                    onPress: async () => {
                        try {
                            await api.restartAgent(agent.session_id);
                            onBack();
                        } catch (e) {
                            Alert.alert("Error", "Failed to restart agent");
                        }
                    },
                },
            ],
        );
    };

    const toggleToolExpanded = (id: number) => {
        setExpandedTools((prev) => {
            const next = new Set(prev);
            if (next.has(id)) {
                next.delete(id);
            } else {
                next.add(id);
            }
            return next;
        });
    };

    const copyToClipboard = async (text: string) => {
        await Clipboard.setStringAsync(text);
        Alert.alert("Copied", "Message copied to clipboard");
    };

    const renderMessage = ({ item }: { item: Message }) => {
        const isUser = item.role === "user";
        const isTool = item.role === "tool";
        const isExpanded = expandedTools.has(item.id);

        // For tool messages, show collapsed by default
        if (isTool) {
            const isRunning = item.content.startsWith("[RUNNING]");
            const isCompleted = item.content.startsWith("[COMPLETED]");
            const isFailed = item.content.startsWith("[FAILED]");

            // Strip the status prefix for display
            let displayContent = item.content;
            if (isRunning) {
                displayContent = item.content.replace(/^\[RUNNING\]\s*/, "");
            } else if (isCompleted) {
                displayContent = item.content.replace(/^\[COMPLETED\]\s*/, "");
            } else if (isFailed) {
                displayContent = item.content.replace(/^\[FAILED\]\s*/, "");
            }

            // Detect file operations and parse them
            const fileOpMatch = displayContent.match(
                /^(Read|Write|Edit|mcp__acp__Read|mcp__acp__Write|mcp__acp__Edit):\s*(.+)/,
            );
            const isFileOp = !!fileOpMatch;
            const toolName = fileOpMatch?.[1]?.replace("mcp__acp__", "") || "";
            const filePath = fileOpMatch?.[2]?.split("\n")[0] || "";
            const fileContent = isFileOp
                ? displayContent.substring(displayContent.indexOf("\n") + 1)
                : "";

            // Get appropriate icon for tool type
            const getToolIcon = () => {
                if (isRunning) return "⏳";
                if (isFailed) return "✗";
                if (isFileOp) {
                    if (toolName === "Read") return "📖";
                    if (toolName === "Write") return "📝";
                    if (toolName === "Edit") return "✏️";
                }
                if (isCompleted) return "✓";
                return "🔧";
            };

            // Extract the command/title (first line or bracket content)
            const firstLine = displayContent.split("\n")[0];
            const preview =
                firstLine.length > 60
                    ? firstLine.slice(0, 57) + "..."
                    : firstLine;

            return (
                <Pressable
                    style={[styles.messageContainer, styles.toolMessage]}
                    onPress={() => toggleToolExpanded(item.id)}
                    onLongPress={() => copyToClipboard(displayContent)}
                >
                    <View style={styles.toolHeader}>
                        <Text style={styles.toolIcon}>{getToolIcon()}</Text>
                        {isFileOp && !isExpanded ? (
                            <View style={styles.fileOpHeader}>
                                <Text
                                    style={[
                                        styles.toolPreview,
                                        isCompleted && styles.toolCompleted,
                                        isFailed && styles.toolFailed,
                                    ]}
                                >
                                    {toolName}
                                </Text>
                                <Text style={styles.filePath} numberOfLines={1}>
                                    {filePath.replace(/^\/Users\/[^/]+/, "~")}
                                </Text>
                            </View>
                        ) : (
                            <Text
                                style={[
                                    styles.toolPreview,
                                    isCompleted && styles.toolCompleted,
                                    isFailed && styles.toolFailed,
                                ]}
                                numberOfLines={isExpanded ? undefined : 1}
                            >
                                {isExpanded ? displayContent : preview}
                            </Text>
                        )}
                        <Text style={styles.expandIcon}>
                            {isExpanded ? "▼" : "▶"}
                        </Text>
                    </View>
                    {isExpanded && isFileOp && fileContent && (
                        <View style={styles.fileContentContainer}>
                            <Text style={styles.fileContentText}>
                                {fileContent}
                            </Text>
                        </View>
                    )}
                    {!isExpanded &&
                        (displayContent.includes("\n") ||
                            (isFileOp && isCompleted)) && (
                            <Text style={styles.moreIndicator}>
                                {isFileOp && isCompleted
                                    ? "tap to view content"
                                    : "tap to expand"}
                            </Text>
                        )}
                </Pressable>
            );
        }

        return (
            <Pressable
                style={[
                    styles.messageContainer,
                    isUser ? styles.userMessage : styles.agentMessage,
                ]}
                onLongPress={() => copyToClipboard(item.content)}
            >
                <Text style={styles.roleLabel}>{isUser ? "You" : "Agent"}</Text>
                {isUser ? (
                    <Text style={styles.messageText}>{item.content}</Text>
                ) : (
                    <View style={styles.markdownContainer}>
                        <Markdown style={markdownStyles}>
                            {item.content}
                        </Markdown>
                    </View>
                )}
                <View style={styles.timestampRow}>
                    <Text style={styles.timestamp}>
                        {formatTime(item.timestamp)}
                    </Text>
                    {isUser && item.status && (
                        <Text style={styles.deliveryStatus}>
                            {item.status === "sending" ? "○" : "✓"}
                        </Text>
                    )}
                </View>
            </Pressable>
        );
    };

    return (
        <KeyboardAvoidingView
            style={styles.container}
            behavior={Platform.OS === "ios" ? "padding" : "height"}
            keyboardVerticalOffset={Platform.OS === "ios" ? 90 : 0}
        >
            {/* Header */}
            <View style={styles.header}>
                <TouchableOpacity onPress={onBack} style={styles.backButton}>
                    <Text style={styles.backText}>←</Text>
                </TouchableOpacity>
                <View style={styles.headerInfo}>
                    <Text style={styles.headerTitle} numberOfLines={1}>
                        {agent.buffer_name.split(" @ ")[0]}
                    </Text>
                    <Text style={styles.headerSubtitle}>{agent.project}</Text>
                </View>
                <View style={styles.headerActions}>
                    <TouchableOpacity
                        onPress={() => setShowFileExplorer(true)}
                        style={styles.filesButton}
                    >
                        <Text style={styles.filesButtonText}>📁</Text>
                    </TouchableOpacity>
                    {agent.status === "processing" && (
                        <TouchableOpacity
                            onPress={handleStop}
                            style={styles.stopButton}
                        >
                            <Text style={styles.stopText}>Stop</Text>
                        </TouchableOpacity>
                    )}
                    <TouchableOpacity
                        onPress={handleRestart}
                        style={styles.restartButton}
                    >
                        <Text style={styles.restartButtonText}>↻</Text>
                    </TouchableOpacity>
                    <TouchableOpacity
                        onPress={handleClose}
                        style={styles.closeAgentButton}
                    >
                        <Text style={styles.closeAgentText}>End</Text>
                    </TouchableOpacity>
                </View>
            </View>

            {/* File Explorer Modal */}
            <Modal
                visible={showFileExplorer}
                animationType="slide"
                onRequestClose={() => setShowFileExplorer(false)}
            >
                <FileExplorerScreen
                    initialPath={agent.project_path || agent.project}
                    onClose={() => setShowFileExplorer(false)}
                />
            </Modal>

            {/* Messages */}
            {loading && messages.length === 0 ? (
                <View style={styles.centered}>
                    <ActivityIndicator size="large" color="#007AFF" />
                </View>
            ) : error ? (
                <View style={styles.centered}>
                    <Text style={styles.errorText}>{error}</Text>
                </View>
            ) : (
                <>
                    <FlatList
                        ref={flatListRef}
                        data={messages}
                        keyExtractor={(item) => String(item.id)}
                        renderItem={renderMessage}
                        contentContainerStyle={styles.messagesList}
                        onScroll={handleScroll}
                        scrollEventThrottle={100}
                        onContentSizeChange={handleContentSizeChange}
                    />
                    {showScrollButton && (
                        <TouchableOpacity
                            style={styles.floatingScrollButton}
                            onPress={scrollToBottom}
                        >
                            <Text style={styles.floatingScrollButtonText}>
                                ↓
                            </Text>
                        </TouchableOpacity>
                    )}
                </>
            )}

            {/* Input */}
            <View style={styles.inputContainer}>
                <TextInput
                    style={styles.input}
                    value={inputText}
                    onChangeText={setInputText}
                    placeholder="Message the agent..."
                    placeholderTextColor="#666666"
                    multiline
                    maxLength={4000}
                    editable={!sending}
                />
                <TouchableOpacity
                    style={[
                        styles.sendButton,
                        (!inputText.trim() || sending) &&
                            styles.sendButtonDisabled,
                    ]}
                    onPress={handleSend}
                    disabled={!inputText.trim() || sending}
                >
                    {sending ? (
                        <ActivityIndicator size="small" color="#FFFFFF" />
                    ) : (
                        <Text style={styles.sendButtonText}>Send</Text>
                    )}
                </TouchableOpacity>
            </View>
        </KeyboardAvoidingView>
    );
}

function formatTime(isoString: string): string {
    const date = new Date(isoString);
    return date.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
}

const markdownStyles = StyleSheet.create({
    body: {
        color: "#FFFFFF",
        fontSize: 15,
        lineHeight: 22,
    },
    heading1: {
        color: "#FFFFFF",
        fontSize: 22,
        fontWeight: "700",
        marginTop: 12,
        marginBottom: 8,
    },
    heading2: {
        color: "#FFFFFF",
        fontSize: 19,
        fontWeight: "600",
        marginTop: 10,
        marginBottom: 6,
    },
    heading3: {
        color: "#FFFFFF",
        fontSize: 17,
        fontWeight: "600",
        marginTop: 8,
        marginBottom: 4,
    },
    code_inline: {
        backgroundColor: "#1a1a2e",
        color: "#e06c75",
        fontFamily: Platform.OS === "ios" ? "Menlo" : "monospace",
        fontSize: 13,
        paddingHorizontal: 4,
        borderRadius: 3,
    },
    fence: {
        backgroundColor: "#1a1a2e",
        color: "#abb2bf",
        fontFamily: Platform.OS === "ios" ? "Menlo" : "monospace",
        fontSize: 13,
        padding: 10,
        borderRadius: 6,
        marginVertical: 8,
    },
    code_block: {
        backgroundColor: "#1a1a2e",
        color: "#abb2bf",
        fontFamily: Platform.OS === "ios" ? "Menlo" : "monospace",
        fontSize: 13,
        padding: 10,
        borderRadius: 6,
        marginVertical: 8,
    },
    blockquote: {
        backgroundColor: "#1a1a2e",
        borderLeftWidth: 3,
        borderLeftColor: "#007AFF",
        paddingLeft: 12,
        marginVertical: 8,
    },
    bullet_list: {
        marginVertical: 4,
    },
    ordered_list: {
        marginVertical: 4,
    },
    list_item: {
        flexDirection: "row",
        marginVertical: 2,
        flexShrink: 1,
        flexWrap: "wrap",
    },
    bullet_list_content: {
        flex: 1,
    },
    ordered_list_content: {
        flex: 1,
    },
    paragraph: {
        flexShrink: 1,
        flexWrap: "wrap",
    },
    text: {
        flexShrink: 1,
    },
    link: {
        color: "#007AFF",
        textDecorationLine: "underline",
    },
    strong: {
        fontWeight: "700",
    },
    em: {
        fontStyle: "italic",
    },
});

const styles = StyleSheet.create({
    container: {
        flex: 1,
        backgroundColor: "#121212",
    },
    header: {
        flexDirection: "row",
        alignItems: "center",
        padding: 12,
        backgroundColor: "#1E1E1E",
        borderBottomWidth: 1,
        borderBottomColor: "#333333",
    },
    backButton: {
        padding: 8,
        marginRight: 8,
    },
    backText: {
        color: "#007AFF",
        fontSize: 20,
    },
    headerInfo: {
        flex: 1,
    },
    headerTitle: {
        color: "#FFFFFF",
        fontSize: 16,
        fontWeight: "600",
    },
    headerSubtitle: {
        color: "#888888",
        fontSize: 12,
        marginTop: 2,
    },
    headerActions: {
        flexDirection: "row",
        alignItems: "center",
        gap: 8,
    },
    filesButton: {
        padding: 8,
    },
    filesButtonText: {
        fontSize: 18,
    },
    stopButton: {
        backgroundColor: "#FF9800",
        paddingHorizontal: 12,
        paddingVertical: 6,
        borderRadius: 6,
    },
    stopText: {
        color: "#FFFFFF",
        fontSize: 12,
        fontWeight: "600",
    },
    restartButton: {
        padding: 8,
    },
    restartButtonText: {
        color: "#007AFF",
        fontSize: 18,
    },
    closeAgentButton: {
        paddingHorizontal: 10,
        paddingVertical: 6,
        borderRadius: 6,
        borderWidth: 1,
        borderColor: "#F44336",
    },
    closeAgentText: {
        color: "#F44336",
        fontSize: 12,
        fontWeight: "600",
    },
    floatingScrollButton: {
        position: "absolute",
        bottom: 100,
        right: 16,
        width: 44,
        height: 44,
        borderRadius: 22,
        backgroundColor: "rgba(0, 122, 255, 0.9)",
        justifyContent: "center",
        alignItems: "center",
        shadowColor: "#000",
        shadowOffset: { width: 0, height: 2 },
        shadowOpacity: 0.3,
        shadowRadius: 4,
        elevation: 5,
    },
    floatingScrollButtonText: {
        color: "#FFFFFF",
        fontSize: 20,
        fontWeight: "600",
    },
    centered: {
        flex: 1,
        justifyContent: "center",
        alignItems: "center",
    },
    errorText: {
        color: "#F44336",
        fontSize: 14,
    },
    messagesList: {
        padding: 16,
        paddingBottom: 80,
    },
    messageContainer: {
        marginBottom: 12,
        padding: 12,
        borderRadius: 12,
        maxWidth: "85%",
    },
    userMessage: {
        backgroundColor: "#007AFF",
        alignSelf: "flex-end",
    },
    agentMessage: {
        backgroundColor: "#2D2D2D",
        alignSelf: "flex-start",
        width: "85%",
    },
    markdownContainer: {
        flexShrink: 1,
        width: "100%",
    },
    toolMessage: {
        backgroundColor: "#1A1A2E",
        borderLeftWidth: 3,
        borderLeftColor: "#FF9800",
        alignSelf: "stretch",
        maxWidth: "100%",
    },
    toolHeader: {
        flexDirection: "row",
        alignItems: "flex-start",
    },
    toolIcon: {
        fontSize: 14,
        marginRight: 8,
    },
    toolPreview: {
        flex: 1,
        color: "#CCCCCC",
        fontSize: 13,
        fontFamily: Platform.OS === "ios" ? "Menlo" : "monospace",
    },
    toolCompleted: {
        color: "#4CAF50",
    },
    toolFailed: {
        color: "#F44336",
    },
    expandIcon: {
        color: "#666666",
        fontSize: 10,
        marginLeft: 8,
    },
    moreIndicator: {
        color: "#666666",
        fontSize: 10,
        marginTop: 4,
        fontStyle: "italic",
    },
    fileOpHeader: {
        flex: 1,
    },
    filePath: {
        color: "#888888",
        fontSize: 11,
        fontFamily: Platform.OS === "ios" ? "Menlo" : "monospace",
        marginTop: 2,
    },
    fileContentContainer: {
        marginTop: 8,
        padding: 8,
        backgroundColor: "#0d0d1a",
        borderRadius: 6,
        maxHeight: 300,
    },
    fileContentText: {
        color: "#e0e0e0",
        fontSize: 11,
        fontFamily: Platform.OS === "ios" ? "Menlo" : "monospace",
        lineHeight: 16,
    },
    roleLabel: {
        color: "#AAAAAA",
        fontSize: 11,
        fontWeight: "600",
        marginBottom: 4,
    },
    messageText: {
        color: "#FFFFFF",
        fontSize: 15,
        lineHeight: 22,
    },
    timestampRow: {
        flexDirection: "row",
        justifyContent: "flex-end",
        alignItems: "center",
        marginTop: 6,
        gap: 4,
    },
    timestamp: {
        color: "#888888",
        fontSize: 10,
    },
    deliveryStatus: {
        color: "#4CAF50",
        fontSize: 10,
    },
    inputContainer: {
        flexDirection: "row",
        padding: 12,
        backgroundColor: "#1E1E1E",
        borderTopWidth: 1,
        borderTopColor: "#333333",
        alignItems: "flex-end",
    },
    input: {
        flex: 1,
        backgroundColor: "#2D2D2D",
        borderRadius: 20,
        paddingHorizontal: 16,
        paddingVertical: 10,
        color: "#FFFFFF",
        fontSize: 16,
        maxHeight: 100,
        marginRight: 10,
    },
    sendButton: {
        backgroundColor: "#007AFF",
        borderRadius: 20,
        paddingHorizontal: 20,
        paddingVertical: 10,
        justifyContent: "center",
        alignItems: "center",
        minWidth: 60,
    },
    sendButtonDisabled: {
        backgroundColor: "#333333",
    },
    sendButtonText: {
        color: "#FFFFFF",
        fontSize: 16,
        fontWeight: "600",
    },
});
