import React from 'react';
import { View, Text, TouchableOpacity, StyleSheet } from 'react-native';
import { Agent } from '../api/client';

interface AgentNodeProps {
  agent: Agent;
  selected: boolean;
  onPress: () => void;
}

const STATUS_COLORS = {
  ready: '#4CAF50',
  processing: '#FF9800',
  permission_required: '#F44336',
  closed: '#9E9E9E',
};

export function AgentNode({ agent, selected, onPress }: AgentNodeProps) {
  const statusColor = STATUS_COLORS[agent.status] || STATUS_COLORS.ready;
  
  // Extract agent name from buffer name (e.g., "Claude Code Agent @ myproject" -> "Claude Code Agent")
  const agentName = agent.buffer_name.split(' @ ')[0] || agent.buffer_name;
  
  // Truncate last message for preview
  const preview = agent.last_message 
    ? agent.last_message.slice(0, 60) + (agent.last_message.length > 60 ? '...' : '')
    : 'No messages yet';

  return (
    <TouchableOpacity 
      style={[
        styles.container, 
        selected && styles.selected,
        { borderLeftColor: statusColor }
      ]} 
      onPress={onPress}
      activeOpacity={0.7}
    >
      <View style={styles.header}>
        <View style={[styles.statusDot, { backgroundColor: statusColor }]} />
        <Text style={styles.name} numberOfLines={1}>{agentName}</Text>
      </View>
      
      <Text style={styles.project}>{agent.project}</Text>
      
      <Text style={styles.preview} numberOfLines={2}>
        {agent.last_message_role === 'user' ? '→ ' : '← '}
        {preview}
      </Text>
      
      <Text style={styles.time}>
        {formatRelativeTime(agent.last_activity)}
      </Text>
    </TouchableOpacity>
  );
}

function formatRelativeTime(isoString: string): string {
  const date = new Date(isoString);
  const now = new Date();
  const diffMs = now.getTime() - date.getTime();
  const diffMins = Math.floor(diffMs / 60000);
  
  if (diffMins < 1) return 'just now';
  if (diffMins < 60) return `${diffMins}m ago`;
  
  const diffHours = Math.floor(diffMins / 60);
  if (diffHours < 24) return `${diffHours}h ago`;
  
  const diffDays = Math.floor(diffHours / 24);
  return `${diffDays}d ago`;
}

const styles = StyleSheet.create({
  container: {
    backgroundColor: '#1E1E1E',
    borderRadius: 12,
    padding: 16,
    marginVertical: 6,
    marginHorizontal: 12,
    borderLeftWidth: 4,
  },
  selected: {
    backgroundColor: '#2D2D2D',
    borderColor: '#007AFF',
    borderWidth: 1,
  },
  header: {
    flexDirection: 'row',
    alignItems: 'center',
    marginBottom: 4,
  },
  statusDot: {
    width: 8,
    height: 8,
    borderRadius: 4,
    marginRight: 8,
  },
  name: {
    color: '#FFFFFF',
    fontSize: 16,
    fontWeight: '600',
    flex: 1,
  },
  project: {
    color: '#888888',
    fontSize: 12,
    marginBottom: 8,
    marginLeft: 16,
  },
  preview: {
    color: '#CCCCCC',
    fontSize: 14,
    lineHeight: 20,
    marginBottom: 8,
  },
  time: {
    color: '#666666',
    fontSize: 11,
    textAlign: 'right',
  },
});
