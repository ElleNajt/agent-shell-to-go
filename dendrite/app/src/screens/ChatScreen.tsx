import React, { useState, useRef, useEffect } from 'react';
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
} from 'react-native';
import Markdown from 'react-native-markdown-display';
import { useMessages } from '../hooks/useMessages';
import { Agent, Message, api } from '../api/client';
import { FileExplorerScreen } from './FileExplorerScreen';

interface ChatScreenProps {
  agent: Agent;
  onBack: () => void;
}

export function ChatScreen({ agent, onBack }: ChatScreenProps) {
  const { messages, loading, error, sending, sendMessage } = useMessages(agent.session_id);
  const [inputText, setInputText] = useState('');
  const [expandedTools, setExpandedTools] = useState<Set<number>>(new Set());
  const [showFileExplorer, setShowFileExplorer] = useState(false);
  const flatListRef = useRef<FlatList>(null);

  const scrollToBottom = () => {
    flatListRef.current?.scrollToEnd({ animated: true });
  };

  const handleSend = () => {
    if (inputText.trim() && !sending) {
      sendMessage(inputText.trim());
      setInputText('');
    }
  };

  const handleStop = async () => {
    try {
      await api.stopAgent(agent.session_id);
    } catch (e) {
      Alert.alert('Error', 'Failed to stop agent');
    }
  };

  const handleClose = () => {
    Alert.alert(
      'Close Agent',
      `Are you sure you want to close "${agent.buffer_name.split(' @ ')[0]}"?`,
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Close',
          style: 'destructive',
          onPress: async () => {
            try {
              await api.closeAgent(agent.session_id);
              onBack();
            } catch (e) {
              Alert.alert('Error', 'Failed to close agent');
            }
          },
        },
      ]
    );
  };

  const handleRestart = () => {
    // TODO: Use proper acp/agent-shell resume functionality when available
    Alert.alert(
      'Restart Agent',
      `This will restart "${agent.buffer_name.split(' @ ')[0]}" with context from the last 10 messages. Continue?`,
      [
        { text: 'Cancel', style: 'cancel' },
        {
          text: 'Restart',
          onPress: async () => {
            try {
              await api.restartAgent(agent.session_id);
              onBack();
            } catch (e) {
              Alert.alert('Error', 'Failed to restart agent');
            }
          },
        },
      ]
    );
  };

  const toggleToolExpanded = (id: number) => {
    setExpandedTools(prev => {
      const next = new Set(prev);
      if (next.has(id)) {
        next.delete(id);
      } else {
        next.add(id);
      }
      return next;
    });
  };

  const renderMessage = ({ item }: { item: Message }) => {
    const isUser = item.role === 'user';
    const isTool = item.role === 'tool';
    const isExpanded = expandedTools.has(item.id);

    // For tool messages, show collapsed by default
    if (isTool) {
      const isRunning = item.content.startsWith('[RUNNING]');
      const isCompleted = item.content.startsWith('[COMPLETED]');
      const isFailed = item.content.startsWith('[FAILED]');
      
      // Extract the command/title (first line or bracket content)
      const firstLine = item.content.split('\n')[0];
      const preview = firstLine.length > 60 ? firstLine.slice(0, 57) + '...' : firstLine;
      
      return (
        <TouchableOpacity 
          style={[styles.messageContainer, styles.toolMessage]}
          onPress={() => toggleToolExpanded(item.id)}
          activeOpacity={0.7}
        >
          <View style={styles.toolHeader}>
            <Text style={styles.toolIcon}>
              {isRunning ? '⏳' : isCompleted ? '✓' : isFailed ? '✗' : '🔧'}
            </Text>
            <Text style={[
              styles.toolPreview,
              isCompleted && styles.toolCompleted,
              isFailed && styles.toolFailed,
            ]} numberOfLines={isExpanded ? undefined : 1}>
              {isExpanded ? item.content : preview}
            </Text>
            <Text style={styles.expandIcon}>{isExpanded ? '▼' : '▶'}</Text>
          </View>
          {!isExpanded && item.content.includes('\n') && (
            <Text style={styles.moreIndicator}>tap to expand</Text>
          )}
        </TouchableOpacity>
      );
    }

    return (
      <View style={[
        styles.messageContainer,
        isUser ? styles.userMessage : styles.agentMessage,
      ]}>
        <Text style={styles.roleLabel}>
          {isUser ? 'You' : 'Agent'}
        </Text>
        {isUser ? (
          <Text style={styles.messageText}>{item.content}</Text>
        ) : (
          <Markdown style={markdownStyles}>{item.content}</Markdown>
        )}
        <Text style={styles.timestamp}>
          {formatTime(item.timestamp)}
        </Text>
      </View>
    );
  };

  return (
    <KeyboardAvoidingView 
      style={styles.container}
      behavior={Platform.OS === 'ios' ? 'padding' : 'height'}
      keyboardVerticalOffset={Platform.OS === 'ios' ? 90 : 0}
    >
      {/* Header */}
      <View style={styles.header}>
        <TouchableOpacity onPress={onBack} style={styles.backButton}>
          <Text style={styles.backText}>←</Text>
        </TouchableOpacity>
        <View style={styles.headerInfo}>
          <Text style={styles.headerTitle} numberOfLines={1}>
            {agent.buffer_name.split(' @ ')[0]}
          </Text>
          <Text style={styles.headerSubtitle}>{agent.project}</Text>
        </View>
        <View style={styles.headerActions}>
          <TouchableOpacity onPress={() => setShowFileExplorer(true)} style={styles.filesButton}>
            <Text style={styles.filesButtonText}>📁</Text>
          </TouchableOpacity>
          {agent.status === 'processing' && (
            <TouchableOpacity onPress={handleStop} style={styles.stopButton}>
              <Text style={styles.stopText}>Stop</Text>
            </TouchableOpacity>
          )}
          <TouchableOpacity onPress={handleRestart} style={styles.restartButton}>
            <Text style={styles.restartButtonText}>↻</Text>
          </TouchableOpacity>
          <TouchableOpacity onPress={handleClose} style={styles.closeAgentButton}>
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
          initialPath={agent.project}
          onClose={() => setShowFileExplorer(false)}
        />
      </Modal>

      {/* Status bar with jump to bottom */}
      <View style={[styles.statusBar, { backgroundColor: getStatusColor(agent.status) }]}>
        <Text style={styles.statusBarText}>{agent.status}</Text>
        <TouchableOpacity onPress={scrollToBottom} style={styles.jumpButton}>
          <Text style={styles.jumpButtonText}>↓ Bottom</Text>
        </TouchableOpacity>
      </View>

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
        <FlatList
          ref={flatListRef}
          data={messages}
          keyExtractor={(item) => String(item.id)}
          renderItem={renderMessage}
          contentContainerStyle={styles.messagesList}
        />
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
          style={[styles.sendButton, (!inputText.trim() || sending) && styles.sendButtonDisabled]}
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

function getStatusColor(status: string): string {
  switch (status) {
    case 'ready': return '#4CAF50';
    case 'processing': return '#FF9800';
    case 'permission_required': return '#F44336';
    default: return '#9E9E9E';
  }
}

function formatTime(isoString: string): string {
  const date = new Date(isoString);
  return date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
}

const markdownStyles = StyleSheet.create({
  body: {
    color: '#FFFFFF',
    fontSize: 15,
    lineHeight: 22,
  },
  heading1: {
    color: '#FFFFFF',
    fontSize: 22,
    fontWeight: '700',
    marginTop: 12,
    marginBottom: 8,
  },
  heading2: {
    color: '#FFFFFF',
    fontSize: 19,
    fontWeight: '600',
    marginTop: 10,
    marginBottom: 6,
  },
  heading3: {
    color: '#FFFFFF',
    fontSize: 17,
    fontWeight: '600',
    marginTop: 8,
    marginBottom: 4,
  },
  code_inline: {
    backgroundColor: '#1a1a2e',
    color: '#e06c75',
    fontFamily: Platform.OS === 'ios' ? 'Menlo' : 'monospace',
    fontSize: 13,
    paddingHorizontal: 4,
    borderRadius: 3,
  },
  fence: {
    backgroundColor: '#1a1a2e',
    color: '#abb2bf',
    fontFamily: Platform.OS === 'ios' ? 'Menlo' : 'monospace',
    fontSize: 13,
    padding: 10,
    borderRadius: 6,
    marginVertical: 8,
  },
  code_block: {
    backgroundColor: '#1a1a2e',
    color: '#abb2bf',
    fontFamily: Platform.OS === 'ios' ? 'Menlo' : 'monospace',
    fontSize: 13,
    padding: 10,
    borderRadius: 6,
    marginVertical: 8,
  },
  blockquote: {
    backgroundColor: '#1a1a2e',
    borderLeftWidth: 3,
    borderLeftColor: '#007AFF',
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
    flexDirection: 'row',
    marginVertical: 2,
  },
  link: {
    color: '#007AFF',
    textDecorationLine: 'underline',
  },
  strong: {
    fontWeight: '700',
  },
  em: {
    fontStyle: 'italic',
  },
});

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
  },
  header: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 12,
    backgroundColor: '#1E1E1E',
    borderBottomWidth: 1,
    borderBottomColor: '#333333',
  },
  backButton: {
    padding: 8,
    marginRight: 8,
  },
  backText: {
    color: '#007AFF',
    fontSize: 20,
  },
  headerInfo: {
    flex: 1,
  },
  headerTitle: {
    color: '#FFFFFF',
    fontSize: 16,
    fontWeight: '600',
  },
  headerSubtitle: {
    color: '#888888',
    fontSize: 12,
    marginTop: 2,
  },
  headerActions: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 8,
  },
  filesButton: {
    padding: 8,
  },
  filesButtonText: {
    fontSize: 18,
  },
  stopButton: {
    backgroundColor: '#FF9800',
    paddingHorizontal: 12,
    paddingVertical: 6,
    borderRadius: 6,
  },
  stopText: {
    color: '#FFFFFF',
    fontSize: 12,
    fontWeight: '600',
  },
  restartButton: {
    padding: 8,
  },
  restartButtonText: {
    color: '#007AFF',
    fontSize: 18,
  },
  closeAgentButton: {
    paddingHorizontal: 10,
    paddingVertical: 6,
    borderRadius: 6,
    borderWidth: 1,
    borderColor: '#F44336',
  },
  closeAgentText: {
    color: '#F44336',
    fontSize: 12,
    fontWeight: '600',
  },
  statusBar: {
    flexDirection: 'row',
    paddingVertical: 4,
    paddingHorizontal: 12,
    alignItems: 'center',
    justifyContent: 'space-between',
  },
  statusBarText: {
    color: '#FFFFFF',
    fontSize: 11,
    fontWeight: '600',
    textTransform: 'uppercase',
  },
  jumpButton: {
    paddingHorizontal: 8,
    paddingVertical: 2,
    backgroundColor: 'rgba(255,255,255,0.2)',
    borderRadius: 4,
  },
  jumpButtonText: {
    color: '#FFFFFF',
    fontSize: 11,
    fontWeight: '600',
  },
  centered: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  errorText: {
    color: '#F44336',
    fontSize: 14,
  },
  messagesList: {
    padding: 16,
  },
  messageContainer: {
    marginBottom: 12,
    padding: 12,
    borderRadius: 12,
    maxWidth: '85%',
  },
  userMessage: {
    backgroundColor: '#007AFF',
    alignSelf: 'flex-end',
  },
  agentMessage: {
    backgroundColor: '#2D2D2D',
    alignSelf: 'flex-start',
  },
  toolMessage: {
    backgroundColor: '#1A1A2E',
    borderLeftWidth: 3,
    borderLeftColor: '#FF9800',
    alignSelf: 'stretch',
    maxWidth: '100%',
  },
  toolHeader: {
    flexDirection: 'row',
    alignItems: 'flex-start',
  },
  toolIcon: {
    fontSize: 14,
    marginRight: 8,
  },
  toolPreview: {
    flex: 1,
    color: '#CCCCCC',
    fontSize: 13,
    fontFamily: Platform.OS === 'ios' ? 'Menlo' : 'monospace',
  },
  toolCompleted: {
    color: '#4CAF50',
  },
  toolFailed: {
    color: '#F44336',
  },
  expandIcon: {
    color: '#666666',
    fontSize: 10,
    marginLeft: 8,
  },
  moreIndicator: {
    color: '#666666',
    fontSize: 10,
    marginTop: 4,
    fontStyle: 'italic',
  },
  roleLabel: {
    color: '#AAAAAA',
    fontSize: 11,
    fontWeight: '600',
    marginBottom: 4,
  },
  messageText: {
    color: '#FFFFFF',
    fontSize: 15,
    lineHeight: 22,
  },
  timestamp: {
    color: '#888888',
    fontSize: 10,
    marginTop: 6,
    textAlign: 'right',
  },
  inputContainer: {
    flexDirection: 'row',
    padding: 12,
    backgroundColor: '#1E1E1E',
    borderTopWidth: 1,
    borderTopColor: '#333333',
    alignItems: 'flex-end',
  },
  input: {
    flex: 1,
    backgroundColor: '#2D2D2D',
    borderRadius: 20,
    paddingHorizontal: 16,
    paddingVertical: 10,
    color: '#FFFFFF',
    fontSize: 16,
    maxHeight: 100,
    marginRight: 10,
  },
  sendButton: {
    backgroundColor: '#007AFF',
    borderRadius: 20,
    paddingHorizontal: 20,
    paddingVertical: 10,
    justifyContent: 'center',
    alignItems: 'center',
    minWidth: 60,
  },
  sendButtonDisabled: {
    backgroundColor: '#333333',
  },
  sendButtonText: {
    color: '#FFFFFF',
    fontSize: 16,
    fontWeight: '600',
  },
});
