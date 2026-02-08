import React, { useState, useEffect } from 'react';
import { StatusBar } from 'expo-status-bar';
import { SafeAreaView, StyleSheet } from 'react-native';
import AsyncStorage from '@react-native-async-storage/async-storage';

import { api } from './src/api/client';
import { Agent } from './src/api/client';
import { SettingsScreen } from './src/screens/SettingsScreen';
import { AgentsScreen } from './src/screens/AgentsScreen';
import { ChatScreen } from './src/screens/ChatScreen';

type Screen = 'settings' | 'agents' | 'chat';

export default function App() {
  const [screen, setScreen] = useState<Screen>('settings');
  const [selectedAgent, setSelectedAgent] = useState<Agent | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    checkExistingConfig();
  }, []);

  const checkExistingConfig = async () => {
    try {
      const url = await AsyncStorage.getItem('agent_shell_backend_url');
      const token = await AsyncStorage.getItem('agent_shell_backend_token');
      
      if (url && token) {
        api.configure(url, token);
        api.connectWebSocket();
        setScreen('agents');
      }
    } catch (e) {
      console.error('Failed to check config:', e);
    } finally {
      setLoading(false);
    }
  };

  const handleConfigured = () => {
    setScreen('agents');
  };

  const handleSelectAgent = (agent: Agent) => {
    setSelectedAgent(agent);
    setScreen('chat');
  };

  const handleBackFromChat = () => {
    setScreen('agents');
    setSelectedAgent(null);
  };

  if (loading) {
    return null; // Or a splash screen
  }

  return (
    <SafeAreaView style={styles.container}>
      <StatusBar style="light" />
      
      {screen === 'settings' && (
        <SettingsScreen onConfigured={handleConfigured} />
      )}
      
      {screen === 'agents' && (
        <AgentsScreen 
          selectedAgent={selectedAgent}
          onSelectAgent={handleSelectAgent}
        />
      )}
      
      {screen === 'chat' && selectedAgent && (
        <ChatScreen 
          agent={selectedAgent}
          onBack={handleBackFromChat}
        />
      )}
    </SafeAreaView>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
  },
});
