import React, { useState, useEffect } from 'react';
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
import AsyncStorage from '@react-native-async-storage/async-storage';
import { useAgents } from '../hooks/useAgents';
import { GraphView } from '../components/GraphView';
import { Agent, api } from '../api/client';

interface Machine {
  name: string;
  url: string;
}

interface AgentsScreenProps {
  selectedAgent: Agent | null;
  onSelectAgent: (agent: Agent) => void;
}

export function AgentsScreen({ selectedAgent, onSelectAgent }: AgentsScreenProps) {
  const { agents, loading, error, refetch } = useAgents();
  const [showActions, setShowActions] = useState(false);
  const [showNewAgent, setShowNewAgent] = useState(false);
  const [showNewDispatcher, setShowNewDispatcher] = useState(false);
  const [showMachines, setShowMachines] = useState(false);
  const [showAddMachine, setShowAddMachine] = useState(false);
  const [agentName, setAgentName] = useState('');
  const [agentPath, setAgentPath] = useState('');
  const [agentTask, setAgentTask] = useState('');
  const [dispatcherPath, setDispatcherPath] = useState('');
  const [projects, setProjects] = useState<string[]>([]);
  const [machines, setMachines] = useState<Machine[]>([]);
  const [currentMachine, setCurrentMachine] = useState<Machine | null>(null);
  const [newMachineName, setNewMachineName] = useState('');
  const [newMachineUrl, setNewMachineUrl] = useState('');

  useEffect(() => {
    // Small delay to ensure api is configured
    const timer = setTimeout(() => loadMachines(), 100);
    return () => clearTimeout(timer);
  }, []);

  const loadMachines = async () => {
    try {
      const stored = await AsyncStorage.getItem('agent_shell_machines');
      if (stored) {
        const list: Machine[] = JSON.parse(stored);
        setMachines(list);
        // Set current machine from current API config
        const currentUrl = api.getBaseUrl();
        const current = list.find(m => m.url === currentUrl);
        if (current) {
          setCurrentMachine(current);
        } else if (currentUrl) {
          // Add current config as a machine
          const defaultMachine = { name: 'Default', url: currentUrl };
          setMachines([defaultMachine, ...list]);
          setCurrentMachine(defaultMachine);
          await AsyncStorage.setItem('agent_shell_machines', JSON.stringify([defaultMachine, ...list]));
        }
      } else {
        // Initialize with current config
        const currentUrl = api.getBaseUrl();
        if (currentUrl) {
          const defaultMachine = { name: 'Default', url: currentUrl };
          setMachines([defaultMachine]);
          setCurrentMachine(defaultMachine);
          await AsyncStorage.setItem('agent_shell_machines', JSON.stringify([defaultMachine]));
        }
      }
    } catch (e) {
      console.error('Failed to load machines:', e);
    }
  };

  const switchMachine = async (machine: Machine) => {
    setCurrentMachine(machine);
    api.configure(machine.url);
    api.disconnectWebSocket();
    api.connectWebSocket();
    setShowMachines(false);
    refetch();
  };

  const addMachine = async () => {
    if (!newMachineName.trim() || !newMachineUrl.trim()) {
      Alert.alert('Error', 'Name and URL are required');
      return;
    }
    const machine: Machine = { 
      name: newMachineName.trim(), 
      url: newMachineUrl.trim().replace(/\/$/, '') // Remove trailing slash
    };
    const updated = [...machines, machine];
    setMachines(updated);
    await AsyncStorage.setItem('agent_shell_machines', JSON.stringify(updated));
    setNewMachineName('');
    setNewMachineUrl('');
    setShowAddMachine(false);
    switchMachine(machine);
  };

  const deleteMachine = async (machine: Machine) => {
    const updated = machines.filter(m => m.url !== machine.url);
    setMachines(updated);
    await AsyncStorage.setItem('agent_shell_machines', JSON.stringify(updated));
    if (currentMachine?.url === machine.url && updated.length > 0) {
      switchMachine(updated[0]);
    }
  };

  const handleBigRedButton = async () => {
    Alert.alert(
      'STOP ALL AGENTS',
      'This will interrupt ALL running agents. Are you sure?',
      [
        { text: 'Cancel', style: 'cancel' },
        { 
          text: 'STOP ALL', 
          style: 'destructive', 
          onPress: async () => {
            try {
              await api.bigRedButton();
            } catch (e) {
              Alert.alert('Error', 'Failed to stop agents');
            }
          }
        }
      ]
    );
  };

  const handlePruneSessions = async () => {
    try {
      await api.pruneSessions();
      // The backend will broadcast close events for pruned sessions,
      // which will update the UI via WebSocket
    } catch (e) {
      Alert.alert('Error', 'Failed to prune sessions');
    }
  };

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
        <TouchableOpacity 
          style={styles.machineSelector}
          onPress={() => setShowMachines(true)}
        >
          <Text style={styles.title}>{currentMachine?.name || 'Agent Graph'}</Text>
          <Text style={styles.dropdownArrow}>▼</Text>
        </TouchableOpacity>
        <View style={styles.headerButtons}>
          <TouchableOpacity 
            onPress={handleBigRedButton} 
            style={styles.bigRedButton}
          >
            <View style={styles.bigRedButtonInner}>
              <Text style={styles.bigRedButtonText}>STOP</Text>
            </View>
          </TouchableOpacity>
          <TouchableOpacity onPress={handlePruneSessions} style={styles.headerButton}>
            <Text style={styles.headerButtonText}>🧹</Text>
          </TouchableOpacity>
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

      {/* Machine Selector Modal */}
      <Modal
        visible={showMachines}
        transparent
        animationType="fade"
        onRequestClose={() => setShowMachines(false)}
      >
        <TouchableOpacity 
          style={styles.modalOverlay}
          activeOpacity={1}
          onPress={() => setShowMachines(false)}
        >
          <View style={styles.actionsMenu}>
            <Text style={styles.actionsTitle}>Select Machine</Text>
            
            <ScrollView style={styles.machinesList}>
              {machines.map((machine, i) => (
                <TouchableOpacity 
                  key={i}
                  style={[
                    styles.machineItem,
                    currentMachine?.url === machine.url && styles.machineItemActive
                  ]}
                  onPress={() => switchMachine(machine)}
                  onLongPress={() => {
                    Alert.alert(
                      'Delete Machine',
                      `Remove "${machine.name}"?`,
                      [
                        { text: 'Cancel', style: 'cancel' },
                        { text: 'Delete', style: 'destructive', onPress: () => deleteMachine(machine) }
                      ]
                    );
                  }}
                >
                  <Text style={styles.machineName}>{machine.name}</Text>
                  <Text style={styles.machineUrl}>{machine.url}</Text>
                </TouchableOpacity>
              ))}
            </ScrollView>
            
            <TouchableOpacity 
              style={styles.actionButton}
              onPress={() => {
                setShowMachines(false);
                setShowAddMachine(true);
              }}
            >
              <Text style={styles.actionIcon}>+</Text>
              <Text style={styles.actionText}>Add Machine</Text>
            </TouchableOpacity>
            
            <TouchableOpacity 
              style={styles.cancelButton}
              onPress={() => setShowMachines(false)}
            >
              <Text style={styles.cancelText}>Cancel</Text>
            </TouchableOpacity>
          </View>
        </TouchableOpacity>
      </Modal>

      {/* Add Machine Modal */}
      <Modal
        visible={showAddMachine}
        transparent
        animationType="slide"
        onRequestClose={() => setShowAddMachine(false)}
      >
        <View style={styles.modalOverlay}>
          <View style={styles.formModal}>
            <Text style={styles.formTitle}>Add Machine</Text>
            
            <Text style={styles.label}>Name</Text>
            <TextInput
              style={styles.input}
              value={newMachineName}
              onChangeText={setNewMachineName}
              placeholder="e.g., MacBook, Workstation"
              placeholderTextColor="#666"
            />
            
            <Text style={styles.label}>Backend URL</Text>
            <TextInput
              style={styles.input}
              value={newMachineUrl}
              onChangeText={setNewMachineUrl}
              placeholder="e.g., http://100.x.x.x:8080"
              placeholderTextColor="#666"
              autoCapitalize="none"
              keyboardType="url"
            />
            
            <View style={styles.formButtons}>
              <TouchableOpacity 
                style={styles.formCancelButton}
                onPress={() => setShowAddMachine(false)}
              >
                <Text style={styles.formCancelText}>Cancel</Text>
              </TouchableOpacity>
              <TouchableOpacity 
                style={styles.formSubmitButton}
                onPress={addMachine}
              >
                <Text style={styles.formSubmitText}>Add</Text>
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
  machineSelector: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  title: {
    color: '#FFFFFF',
    fontSize: 20,
    fontWeight: '600',
  },
  dropdownArrow: {
    color: '#007AFF',
    fontSize: 12,
    marginLeft: 8,
  },
  machinesList: {
    maxHeight: 200,
    marginBottom: 12,
  },
  machineItem: {
    padding: 12,
    backgroundColor: '#2D2D2D',
    borderRadius: 8,
    marginBottom: 8,
  },
  machineItemActive: {
    borderColor: '#007AFF',
    borderWidth: 2,
  },
  machineName: {
    color: '#FFFFFF',
    fontSize: 16,
    fontWeight: '600',
  },
  machineUrl: {
    color: '#888',
    fontSize: 12,
    marginTop: 4,
  },
  headerButtons: {
    flexDirection: 'row',
    alignItems: 'center',
    gap: 8,
  },
  bigRedButton: {
    width: 52,
    height: 52,
    borderRadius: 26,
    backgroundColor: '#8B0000',
    justifyContent: 'center',
    alignItems: 'center',
    shadowColor: '#FF0000',
    shadowOffset: { width: 0, height: 0 },
    shadowOpacity: 0.5,
    shadowRadius: 8,
    elevation: 8,
    borderWidth: 3,
    borderColor: '#CC0000',
  },
  bigRedButtonInner: {
    width: 42,
    height: 42,
    borderRadius: 21,
    backgroundColor: '#DC0000',
    justifyContent: 'center',
    alignItems: 'center',
    borderWidth: 2,
    borderTopColor: '#FF4444',
    borderLeftColor: '#FF4444',
    borderBottomColor: '#990000',
    borderRightColor: '#990000',
  },
  bigRedButtonText: {
    color: '#FFFFFF',
    fontSize: 10,
    fontWeight: '900',
    letterSpacing: 0.5,
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
