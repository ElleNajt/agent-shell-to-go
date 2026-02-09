// API client for agent-shell-to-go backend

export interface Agent {
  session_id: string;
  buffer_name: string;
  project: string;
  project_path: string;
  parent_session_id: string | null;
  status: 'ready' | 'processing' | 'permission_required' | 'closed';
  last_message: string;
  last_message_role: 'user' | 'agent' | 'tool';
  last_activity: string;
  created_at: string;
  closed_at: string | null;
}

export interface Message {
  id: number;
  session_id: string;
  role: 'user' | 'agent' | 'tool';
  content: string;
  timestamp: string;
  status?: 'sending' | 'sent';  // For user messages: sending = in flight, sent = server received
}

export interface WSEvent {
  type: 'agent_spawn' | 'agent_close' | 'message' | 'status' | 'send_request';
  payload: any;
}

export interface FileEntry {
  name: string;
  is_dir: boolean;
  size: number;
}

export interface FileContent {
  type: 'text' | 'image';
  content: string;
  path: string;
  mime?: string;
}

class ApiClient {
  private baseUrl: string = '';
  private ws: WebSocket | null = null;
  private wsListeners: Set<(event: WSEvent) => void> = new Set();
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;

  configure(baseUrl: string) {
    this.baseUrl = baseUrl;
  }

  isConfigured(): boolean {
    return !!this.baseUrl;
  }

  getBaseUrl(): string {
    return this.baseUrl;
  }

  private async request<T>(path: string, options: RequestInit = {}): Promise<T> {
    const response = await fetch(`${this.baseUrl}${path}`, {
      ...options,
      headers: {
        'Authorization': 'Bearer NOAUTH',
        'Content-Type': 'application/json',
        ...options.headers,
      },
    });

    if (!response.ok) {
      throw new Error(`API error: ${response.status} ${response.statusText}`);
    }

    return response.json();
  }

  async getAgents(includeClosed = false): Promise<Agent[]> {
    const params = includeClosed ? '?include_closed=true' : '';
    const data = await this.request<{ agents: Agent[] | null }>(`/agents${params}`);
    return data.agents || [];
  }

  async getMessages(sessionId: string, limit = 50): Promise<Message[]> {
    const data = await this.request<{ messages: Message[] | null }>(
      `/agents/${encodeURIComponent(sessionId)}/messages?limit=${limit}`
    );
    return data.messages || [];
  }

  async sendMessage(sessionId: string, content: string): Promise<void> {
    await this.request(`/agents/${encodeURIComponent(sessionId)}/send`, {
      method: 'POST',
      body: JSON.stringify({ content }),
    });
  }

  async stopAgent(sessionId: string): Promise<void> {
    await this.request(`/agents/${encodeURIComponent(sessionId)}/stop`, {
      method: 'POST',
    });
  }

  async closeAgent(sessionId: string): Promise<void> {
    await this.request(`/agents/${encodeURIComponent(sessionId)}/close`, {
      method: 'POST',
    });
  }

  async restartAgent(sessionId: string, resumeMessageCount = 10): Promise<void> {
    // TODO: Use proper acp/agent-shell resume functionality when available
    await this.request(`/agents/${encodeURIComponent(sessionId)}/restart`, {
      method: 'POST',
      body: JSON.stringify({ resume_message_count: resumeMessageCount }),
    });
  }

  async newAgent(name: string, path: string, task?: string): Promise<void> {
    await this.request('/actions/new-agent', {
      method: 'POST',
      body: JSON.stringify({ name, path, task: task || '' }),
    });
  }

  async newDispatcher(path: string): Promise<void> {
    await this.request('/actions/new-dispatcher', {
      method: 'POST',
      body: JSON.stringify({ path }),
    });
  }

  async getProjects(): Promise<string[]> {
    const data = await this.request<{ projects: string[] | null }>('/actions/projects');
    return data.projects || [];
  }

  async bigRedButton(): Promise<void> {
    await this.request('/actions/big-red-button', {
      method: 'POST',
    });
  }

  async syncSessions(): Promise<void> {
    // Triggers full sync: prunes dead sessions AND discovers new ones
    await this.request('/actions/prune-sessions', {
      method: 'POST',
    });
  }

  async listFiles(path: string): Promise<{ files: FileEntry[], path: string }> {
    const data = await this.request<{ files: FileEntry[] | null, path: string }>(
      `/files/list?path=${encodeURIComponent(path)}`
    );
    return { files: data.files || [], path: data.path };
  }

  async readFile(path: string): Promise<FileContent> {
    return this.request<FileContent>(`/files/read?path=${encodeURIComponent(path)}`);
  }

  connectWebSocket() {
    if (this.ws?.readyState === WebSocket.OPEN) {
      console.log('WebSocket already open');
      return;
    }

    // Convert http(s) to ws(s)
    const wsUrl = this.baseUrl.replace(/^http/, 'ws') + '/ws?token=NOAUTH';
    console.log('WebSocket connecting to:', wsUrl);
    
    this.ws = new WebSocket(wsUrl);

    this.ws.onopen = () => {
      console.log('WebSocket connected to:', wsUrl);
      
      if (this.reconnectTimer) {
        clearTimeout(this.reconnectTimer);
        this.reconnectTimer = null;
      }
    };

    this.ws.onmessage = (event) => {
      try {
        const wsEvent: WSEvent = JSON.parse(event.data);
        this.wsListeners.forEach(listener => listener(wsEvent));
      } catch (e) {
        console.error('Failed to parse WebSocket message:', e);
      }
    };

    this.ws.onclose = () => {
      console.log('WebSocket disconnected, reconnecting in 5s...');
      this.ws = null;
      if (!this.reconnectTimer) {
        this.reconnectTimer = setTimeout(() => this.connectWebSocket(), 5000);
      }
    };

    this.ws.onerror = (error: any) => {
      console.error('WebSocket error - URL:', wsUrl);
      console.error('WebSocket error - type:', error?.type);
      console.error('WebSocket error - message:', error?.message);
      console.error('WebSocket readyState:', this.ws?.readyState);
      // Close will be called after error, which triggers reconnect
    };
  }

  disconnectWebSocket() {
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  subscribe(listener: (event: WSEvent) => void): () => void {
    this.wsListeners.add(listener);
    return () => this.wsListeners.delete(listener);
  }
}

export const api = new ApiClient();
