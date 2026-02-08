// API client for agent-shell-to-go backend

export interface Agent {
  session_id: string;
  buffer_name: string;
  project: string;
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
}

export interface WSEvent {
  type: 'agent_spawn' | 'agent_close' | 'message' | 'status' | 'send_request';
  payload: any;
}

class ApiClient {
  private baseUrl: string = '';
  private token: string = '';
  private ws: WebSocket | null = null;
  private wsListeners: Set<(event: WSEvent) => void> = new Set();
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;

  configure(baseUrl: string, token: string) {
    this.baseUrl = baseUrl;
    this.token = token;
  }

  isConfigured(): boolean {
    return !!this.baseUrl && !!this.token;
  }

  private async request<T>(path: string, options: RequestInit = {}): Promise<T> {
    const response = await fetch(`${this.baseUrl}${path}`, {
      ...options,
      headers: {
        'Authorization': `Bearer ${this.token}`,
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

  connectWebSocket() {
    if (this.ws?.readyState === WebSocket.OPEN) {
      return;
    }

    // Convert http(s) to ws(s), include token in query param
    const wsUrl = this.baseUrl.replace(/^http/, 'ws') + '/ws?token=' + encodeURIComponent(this.token);
    
    this.ws = new WebSocket(wsUrl);

    this.ws.onopen = () => {
      console.log('WebSocket connected');
      
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
      this.reconnectTimer = setTimeout(() => this.connectWebSocket(), 5000);
    };

    this.ws.onerror = (error) => {
      console.error('WebSocket error:', error);
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
