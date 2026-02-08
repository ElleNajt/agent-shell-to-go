import React from 'react';
import { 
  View, 
  Text, 
  FlatList, 
  StyleSheet, 
  ActivityIndicator,
  RefreshControl,
} from 'react-native';
import { useAgents, groupByProject } from '../hooks/useAgents';
import { AgentNode } from '../components/AgentNode';
import { Agent } from '../api/client';

interface AgentsScreenProps {
  selectedAgent: Agent | null;
  onSelectAgent: (agent: Agent) => void;
}

export function AgentsScreen({ selectedAgent, onSelectAgent }: AgentsScreenProps) {
  const { agents, loading, error, refetch } = useAgents();

  if (loading && agents.length === 0) {
    return (
      <View style={styles.centered}>
        <ActivityIndicator size="large" color="#007AFF" />
        <Text style={styles.loadingText}>Loading agents...</Text>
      </View>
    );
  }

  if (error) {
    return (
      <View style={styles.centered}>
        <Text style={styles.errorText}>Error: {error}</Text>
        <Text style={styles.retryText} onPress={refetch}>Tap to retry</Text>
      </View>
    );
  }

  if (agents.length === 0) {
    return (
      <View style={styles.centered}>
        <Text style={styles.emptyText}>No active agents</Text>
        <Text style={styles.hintText}>Start an agent in Emacs to see it here</Text>
      </View>
    );
  }

  // Group by project
  const grouped = groupByProject(agents);
  const sections = Array.from(grouped.entries()).map(([project, projectAgents]) => ({
    project,
    agents: projectAgents,
  }));

  return (
    <View style={styles.container}>
      <FlatList
        data={sections}
        keyExtractor={(item) => item.project}
        refreshControl={
          <RefreshControl 
            refreshing={loading} 
            onRefresh={refetch}
            tintColor="#007AFF"
          />
        }
        renderItem={({ item: section }) => (
          <View style={styles.section}>
            <Text style={styles.sectionHeader}>{section.project}</Text>
            {section.agents.map(agent => (
              <AgentNode
                key={agent.session_id}
                agent={agent}
                selected={selectedAgent?.session_id === agent.session_id}
                onPress={() => onSelectAgent(agent)}
              />
            ))}
          </View>
        )}
      />
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
  },
  centered: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    backgroundColor: '#121212',
    padding: 20,
  },
  loadingText: {
    color: '#888888',
    marginTop: 12,
    fontSize: 16,
  },
  errorText: {
    color: '#F44336',
    fontSize: 16,
    textAlign: 'center',
  },
  retryText: {
    color: '#007AFF',
    marginTop: 12,
    fontSize: 16,
  },
  emptyText: {
    color: '#FFFFFF',
    fontSize: 18,
    fontWeight: '600',
  },
  hintText: {
    color: '#888888',
    marginTop: 8,
    fontSize: 14,
  },
  section: {
    marginTop: 16,
  },
  sectionHeader: {
    color: '#888888',
    fontSize: 13,
    fontWeight: '600',
    textTransform: 'uppercase',
    letterSpacing: 1,
    marginLeft: 16,
    marginBottom: 8,
  },
});
