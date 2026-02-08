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
} from 'react-native';
import { useMessages } from '../hooks/useMessages';
import { Agent, Message } from '../api/client';

interface ChatScreenProps {
  agent: Agent;
  onBack: () => void;
}

export function ChatScreen({ agent, onBack }: ChatScreenProps) {
  const { messages, loading, error, sending, sendMessage } = useMessages(agent.session_id);
  const [inputText, setInputText] = useState('');
  const flatListRef = useRef<FlatList>(null);

  // Scroll to bottom when new messages arrive
  useEffect(() => {
    if (messages.length > 0) {
      setTimeout(() => {
        flatListRef.current?.scrollToEnd({ animated: true });
      }, 100);
    }
  }, [messages.length]);

  const handleSend = () => {
    if (inputText.trim() && !sending) {
      sendMessage(inputText.trim());
      setInputText('');
    }
  };

  const renderMessage = ({ item }: { item: Message }) => {
    const isUser = item.role === 'user';
    const isTool = item.role === 'tool';

    return (
      <View style={[
        styles.messageContainer,
        isUser ? styles.userMessage : styles.agentMessage,
        isTool && styles.toolMessage,
      ]}>
        <Text style={styles.roleLabel}>
          {isUser ? 'You' : isTool ? 'Tool' : 'Agent'}
        </Text>
        <Text style={[
          styles.messageText,
          isTool && styles.toolText,
        ]}>
          {item.content}
        </Text>
        <Text style={styles.timestamp}>
          {formatTime(item.timestamp)}
        </Text>
      </View>
    );
  };

  return (
    <KeyboardAvoidingView 
      style={styles.container}
      behavior={Platform.OS === 'ios' ? 'padding' : undefined}
      keyboardVerticalOffset={Platform.OS === 'ios' ? 90 : 0}
    >
      {/* Header */}
      <View style={styles.header}>
        <TouchableOpacity onPress={onBack} style={styles.backButton}>
          <Text style={styles.backText}>← Back</Text>
        </TouchableOpacity>
        <View style={styles.headerInfo}>
          <Text style={styles.headerTitle} numberOfLines={1}>
            {agent.buffer_name.split(' @ ')[0]}
          </Text>
          <Text style={styles.headerSubtitle}>{agent.project}</Text>
        </View>
        <View style={[styles.statusBadge, { backgroundColor: getStatusColor(agent.status) }]}>
          <Text style={styles.statusText}>{agent.status}</Text>
        </View>
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
          onContentSizeChange={() => flatListRef.current?.scrollToEnd()}
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

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
  },
  header: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 16,
    backgroundColor: '#1E1E1E',
    borderBottomWidth: 1,
    borderBottomColor: '#333333',
  },
  backButton: {
    marginRight: 12,
  },
  backText: {
    color: '#007AFF',
    fontSize: 16,
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
  statusBadge: {
    paddingHorizontal: 8,
    paddingVertical: 4,
    borderRadius: 12,
  },
  statusText: {
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
  toolText: {
    fontFamily: Platform.OS === 'ios' ? 'Menlo' : 'monospace',
    fontSize: 13,
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
