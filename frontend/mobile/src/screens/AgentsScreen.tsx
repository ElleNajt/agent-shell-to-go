import React, { useState } from 'react';
import { 
  View, 
  Text, 
  StyleSheet, 
  ActivityIndicator,
  TouchableOpacity,
  Modal,
  TextInput,
  Alert,
  ScrollView,
} from 'react-native';
import { useAgents } from '../hooks/useAgents';
import { GraphView } from '../components/GraphView';
import { Agent, api } from '../api/client';

interface AgentsScreenProps {
  selectedAgent: Agent | null;
  onSelectAgent: (agent: Agent) => void;
}

export function AgentsScreen({ selectedAgent, onSelectAgent }: AgentsScreenProps) {
  const { agents, loading, error, refetch } = useAgents();
  const [showActions, setShowActions] = useState(false);
  const [showNewAgent, setShowNewAgent] = useState(false);
  const [showNewDispatcher, setShowNewDispatcher] = useState(false);
  const [agentName, setAgentName] = useState('');
  const [agentPath, setAgentPath] = useState('');
  const [agentTask, setAgentTask] = useState('');
  const [dispatcherPath, setDispatcherPath] = useState('');
  const [projects, setProjects] = useState<string[]>([]);

  const loadProjects = async () => {
    try {
      const p = await api.getProjects();
      setProjects(p);
    } catch (e) {
      console.error('Failed to load projects:', e);
    }
  };

  const handleNewAgent = async () => {
    if (!agentName.trim() || !agentPath.trim()) {
      Alert.alert('Error', 'Name and path are required');
      return;
    }
    try {
      await api.newAgent(agentName.trim(), agentPath.trim(), agentTask.trim());
      setShowNewAgent(false);
      setAgentName('');
      setAgentPath('');
      setAgentTask('');
      Alert.alert('Success', 'Agent spawn requested');
    } catch (e) {
      Alert.alert('Error', 'Failed to spawn agent');
    }
  };

  const handleNewDispatcher = async () => {
    if (!dispatcherPath.trim()) {
      Alert.alert('Error', 'Path is required');
      return;
    }
    try {
      await api.newDispatcher(dispatcherPath.trim());
      setShowNewDispatcher(false);
      setDispatcherPath('');
      Alert.alert('Success', 'Dispatcher spawn requested');
    } catch (e) {
      Alert.alert('Error', 'Failed to spawn dispatcher');
    }
  };

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
        <TouchableOpacity onPress={refetch} style={styles.retryButton}>
          <Text style={styles.retryText}>Tap to retry</Text>
        </TouchableOpacity>
      </View>
    );
  }

  return (
    <View style={styles.container}>
      <View style={styles.header}>
        <Text style={styles.title}>Agent Graph</Text>
        <View style={styles.headerButtons}>
          <TouchableOpacity onPress={refetch} style={styles.headerButton}>
            <Text style={styles.headerButtonText}>↻</Text>
          </TouchableOpacity>
          <TouchableOpacity 
            onPress={() => {
              loadProjects();
              setShowActions(true);
            }} 
            style={styles.headerButton}
          >
            <Text style={styles.headerButtonText}>+</Text>
          </TouchableOpacity>
        </View>
      </View>
      
      <GraphView
        agents={agents}
        selectedAgent={selectedAgent}
        onSelectAgent={onSelectAgent}
      />

      {/* Actions Modal */}
      <Modal
        visible={showActions}
        transparent
        animationType="fade"
        onRequestClose={() => setShowActions(false)}
      >
        <TouchableOpacity 
          style={styles.modalOverlay}
          activeOpacity={1}
          onPress={() => setShowActions(false)}
        >
          <View style={styles.actionsMenu}>
            <Text style={styles.actionsTitle}>Actions</Text>
            
            <TouchableOpacity 
              style={styles.actionButton}
              onPress={() => {
                setShowActions(false);
                setShowNewAgent(true);
              }}
            >
              <Text style={styles.actionIcon}>🤖</Text>
              <Text style={styles.actionText}>New Agent</Text>
            </TouchableOpacity>
            
            <TouchableOpacity 
              style={styles.actionButton}
              onPress={() => {
                setShowActions(false);
                setShowNewDispatcher(true);
              }}
            >
              <Text style={styles.actionIcon}>📋</Text>
              <Text style={styles.actionText}>New Dispatcher</Text>
            </TouchableOpacity>
            
            <View style={styles.divider} />
            
            <Text style={styles.projectsTitle}>Active Projects</Text>
            <ScrollView style={styles.projectsList}>
              {projects.length === 0 ? (
                <Text style={styles.noProjects}>No projects</Text>
              ) : (
                projects.map((project, i) => (
                  <TouchableOpacity 
                    key={i}
                    style={styles.projectItem}
                    onPress={() => {
                      setDispatcherPath(`~/code/${project}`);
                      setShowActions(false);
                      setShowNewDispatcher(true);
                    }}
                  >
                    <Text style={styles.projectName}>{project}</Text>
                  </TouchableOpacity>
                ))
              )}
            </ScrollView>
            
            <TouchableOpacity 
              style={styles.cancelButton}
              onPress={() => setShowActions(false)}
            >
              <Text style={styles.cancelText}>Cancel</Text>
            </TouchableOpacity>
          </View>
        </TouchableOpacity>
      </Modal>

      {/* New Agent Modal */}
      <Modal
        visible={showNewAgent}
        transparent
        animationType="slide"
        onRequestClose={() => setShowNewAgent(false)}
      >
        <View style={styles.modalOverlay}>
          <View style={styles.formModal}>
            <Text style={styles.formTitle}>New Agent</Text>
            
            <Text style={styles.label}>Name</Text>
            <TextInput
              style={styles.input}
              value={agentName}
              onChangeText={setAgentName}
              placeholder="e.g., Refactor Agent"
              placeholderTextColor="#666"
            />
            
            <Text style={styles.label}>Project Path</Text>
            <TextInput
              style={styles.input}
              value={agentPath}
              onChangeText={setAgentPath}
              placeholder="e.g., ~/code/myproject"
              placeholderTextColor="#666"
              autoCapitalize="none"
            />
            
            <Text style={styles.label}>Initial Task (optional)</Text>
            <TextInput
              style={[styles.input, styles.taskInput]}
              value={agentTask}
              onChangeText={setAgentTask}
              placeholder="What should this agent do?"
              placeholderTextColor="#666"
              multiline
            />
            
            <View style={styles.formButtons}>
              <TouchableOpacity 
                style={styles.formCancelButton}
                onPress={() => setShowNewAgent(false)}
              >
                <Text style={styles.formCancelText}>Cancel</Text>
              </TouchableOpacity>
              <TouchableOpacity 
                style={styles.formSubmitButton}
                onPress={handleNewAgent}
              >
                <Text style={styles.formSubmitText}>Create</Text>
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </Modal>

      {/* New Dispatcher Modal */}
      <Modal
        visible={showNewDispatcher}
        transparent
        animationType="slide"
        onRequestClose={() => setShowNewDispatcher(false)}
      >
        <View style={styles.modalOverlay}>
          <View style={styles.formModal}>
            <Text style={styles.formTitle}>New Dispatcher</Text>
            
            <Text style={styles.label}>Project Path</Text>
            <TextInput
              style={styles.input}
              value={dispatcherPath}
              onChangeText={setDispatcherPath}
              placeholder="e.g., ~/code/myproject"
              placeholderTextColor="#666"
              autoCapitalize="none"
            />
            
            <View style={styles.formButtons}>
              <TouchableOpacity 
                style={styles.formCancelButton}
                onPress={() => setShowNewDispatcher(false)}
              >
                <Text style={styles.formCancelText}>Cancel</Text>
              </TouchableOpacity>
              <TouchableOpacity 
                style={styles.formSubmitButton}
                onPress={handleNewDispatcher}
              >
                <Text style={styles.formSubmitText}>Create</Text>
              </TouchableOpacity>
            </View>
          </View>
        </View>
      </Modal>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
  },
  header: {
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
    padding: 16,
    borderBottomWidth: 1,
    borderBottomColor: '#333',
  },
  title: {
    color: '#FFFFFF',
    fontSize: 20,
    fontWeight: '600',
  },
  headerButtons: {
    flexDirection: 'row',
    gap: 8,
  },
  headerButton: {
    width: 36,
    height: 36,
    borderRadius: 18,
    backgroundColor: '#2D2D2D',
    justifyContent: 'center',
    alignItems: 'center',
  },
  headerButtonText: {
    color: '#007AFF',
    fontSize: 20,
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
  retryButton: {
    marginTop: 12,
    padding: 12,
  },
  retryText: {
    color: '#007AFF',
    fontSize: 16,
  },
  modalOverlay: {
    flex: 1,
    backgroundColor: 'rgba(0,0,0,0.7)',
    justifyContent: 'center',
    alignItems: 'center',
  },
  actionsMenu: {
    backgroundColor: '#1E1E1E',
    borderRadius: 16,
    padding: 20,
    width: '80%',
    maxWidth: 300,
  },
  actionsTitle: {
    color: '#FFFFFF',
    fontSize: 18,
    fontWeight: '600',
    marginBottom: 16,
    textAlign: 'center',
  },
  actionButton: {
    flexDirection: 'row',
    alignItems: 'center',
    padding: 12,
    backgroundColor: '#2D2D2D',
    borderRadius: 8,
    marginBottom: 8,
  },
  actionIcon: {
    fontSize: 20,
    marginRight: 12,
  },
  actionText: {
    color: '#FFFFFF',
    fontSize: 16,
  },
  divider: {
    height: 1,
    backgroundColor: '#333',
    marginVertical: 12,
  },
  projectsTitle: {
    color: '#888',
    fontSize: 12,
    textTransform: 'uppercase',
    marginBottom: 8,
  },
  projectsList: {
    maxHeight: 150,
  },
  noProjects: {
    color: '#666',
    fontStyle: 'italic',
    textAlign: 'center',
    padding: 12,
  },
  projectItem: {
    padding: 10,
    backgroundColor: '#2D2D2D',
    borderRadius: 6,
    marginBottom: 4,
  },
  projectName: {
    color: '#FFFFFF',
    fontSize: 14,
  },
  cancelButton: {
    marginTop: 12,
    padding: 12,
    alignItems: 'center',
  },
  cancelText: {
    color: '#888',
    fontSize: 16,
  },
  formModal: {
    backgroundColor: '#1E1E1E',
    borderRadius: 16,
    padding: 20,
    width: '90%',
    maxWidth: 400,
  },
  formTitle: {
    color: '#FFFFFF',
    fontSize: 20,
    fontWeight: '600',
    marginBottom: 20,
    textAlign: 'center',
  },
  label: {
    color: '#888',
    fontSize: 12,
    textTransform: 'uppercase',
    marginBottom: 6,
  },
  input: {
    backgroundColor: '#2D2D2D',
    borderRadius: 8,
    padding: 12,
    color: '#FFFFFF',
    fontSize: 16,
    marginBottom: 16,
  },
  taskInput: {
    height: 80,
    textAlignVertical: 'top',
  },
  formButtons: {
    flexDirection: 'row',
    justifyContent: 'flex-end',
    gap: 12,
    marginTop: 8,
  },
  formCancelButton: {
    padding: 12,
  },
  formCancelText: {
    color: '#888',
    fontSize: 16,
  },
  formSubmitButton: {
    backgroundColor: '#007AFF',
    paddingHorizontal: 24,
    paddingVertical: 12,
    borderRadius: 8,
  },
  formSubmitText: {
    color: '#FFFFFF',
    fontSize: 16,
    fontWeight: '600',
  },
});
