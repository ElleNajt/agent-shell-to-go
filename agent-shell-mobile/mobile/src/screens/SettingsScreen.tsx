import React, { useState, useEffect } from 'react';
import {
  View,
  Text,
  TextInput,
  TouchableOpacity,
  StyleSheet,
  Alert,
} from 'react-native';
import AsyncStorage from '@react-native-async-storage/async-storage';
import { api } from '../api/client';

interface SettingsScreenProps {
  onConfigured: () => void;
}

const STORAGE_KEY_URL = 'agent_shell_backend_url';
const STORAGE_KEY_TOKEN = 'agent_shell_backend_token';

export function SettingsScreen({ onConfigured }: SettingsScreenProps) {
  const [url, setUrl] = useState('');
  const [token, setToken] = useState('');
  const [testing, setTesting] = useState(false);

  useEffect(() => {
    loadSavedConfig();
  }, []);

  const loadSavedConfig = async () => {
    try {
      const savedUrl = await AsyncStorage.getItem(STORAGE_KEY_URL);
      const savedToken = await AsyncStorage.getItem(STORAGE_KEY_TOKEN);
      if (savedUrl) setUrl(savedUrl);
      if (savedToken) setToken(savedToken);
    } catch (e) {
      console.error('Failed to load config:', e);
    }
  };

  const testConnection = async () => {
    if (!url.trim() || !token.trim()) {
      Alert.alert('Error', 'Please enter both URL and token');
      return;
    }

    setTesting(true);
    try {
      // Configure API client
      api.configure(url.trim(), token.trim());
      
      // Test by fetching agents
      await api.getAgents();
      
      // Save config
      await AsyncStorage.setItem(STORAGE_KEY_URL, url.trim());
      await AsyncStorage.setItem(STORAGE_KEY_TOKEN, token.trim());
      
      // Connect WebSocket
      api.connectWebSocket();
      
      Alert.alert('Success', 'Connected to backend!', [
        { text: 'OK', onPress: onConfigured }
      ]);
    } catch (e) {
      Alert.alert('Connection Failed', e instanceof Error ? e.message : 'Unknown error');
    } finally {
      setTesting(false);
    }
  };

  return (
    <View style={styles.container}>
      <Text style={styles.title}>Agent Shell Mobile</Text>
      <Text style={styles.subtitle}>Connect to your backend server</Text>

      <View style={styles.form}>
        <Text style={styles.label}>Backend URL</Text>
        <TextInput
          style={styles.input}
          value={url}
          onChangeText={setUrl}
          placeholder="http://100.x.x.x:8080"
          placeholderTextColor="#666666"
          autoCapitalize="none"
          autoCorrect={false}
          keyboardType="url"
        />

        <Text style={styles.label}>Auth Token</Text>
        <TextInput
          style={styles.input}
          value={token}
          onChangeText={setToken}
          placeholder="Your secret token"
          placeholderTextColor="#666666"
          autoCapitalize="none"
          autoCorrect={false}
          secureTextEntry
        />

        <TouchableOpacity
          style={[styles.button, testing && styles.buttonDisabled]}
          onPress={testConnection}
          disabled={testing}
        >
          <Text style={styles.buttonText}>
            {testing ? 'Testing...' : 'Connect'}
          </Text>
        </TouchableOpacity>
      </View>

      <Text style={styles.hint}>
        The backend should be running on your Tailscale network.
        Make sure Tailscale is connected on this device.
      </Text>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
    padding: 24,
    justifyContent: 'center',
  },
  title: {
    color: '#FFFFFF',
    fontSize: 28,
    fontWeight: 'bold',
    textAlign: 'center',
    marginBottom: 8,
  },
  subtitle: {
    color: '#888888',
    fontSize: 16,
    textAlign: 'center',
    marginBottom: 40,
  },
  form: {
    marginBottom: 24,
  },
  label: {
    color: '#AAAAAA',
    fontSize: 14,
    fontWeight: '600',
    marginBottom: 8,
  },
  input: {
    backgroundColor: '#2D2D2D',
    borderRadius: 12,
    padding: 16,
    color: '#FFFFFF',
    fontSize: 16,
    marginBottom: 20,
  },
  button: {
    backgroundColor: '#007AFF',
    borderRadius: 12,
    padding: 16,
    alignItems: 'center',
    marginTop: 12,
  },
  buttonDisabled: {
    backgroundColor: '#333333',
  },
  buttonText: {
    color: '#FFFFFF',
    fontSize: 18,
    fontWeight: '600',
  },
  hint: {
    color: '#666666',
    fontSize: 13,
    textAlign: 'center',
    lineHeight: 20,
  },
});
