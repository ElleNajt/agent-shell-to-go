import { useState, useEffect, useCallback } from 'react';
import { api, Agent, WSEvent } from '../api/client';

export function useAgents() {
  const [agents, setAgents] = useState<Agent[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchAgents = useCallback(async () => {
    try {
      setLoading(true);
      const data = await api.getAgents();
      setAgents(data);
      setError(null);
    } catch (e) {
      setError(e instanceof Error ? e.message : 'Failed to fetch agents');
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    if (!api.isConfigured()) return;
    
    fetchAgents();

    const unsubscribe = api.subscribe((event: WSEvent) => {
      switch (event.type) {
        case 'agent_spawn':
          setAgents(prev => {
            const exists = prev.some(a => a.session_id === event.payload.session_id);
            if (exists) return prev;
            return [...prev, {
              session_id: event.payload.session_id,
              buffer_name: event.payload.buffer_name,
              project: event.payload.project,
              parent_session_id: event.payload.parent_session_id,
              status: 'ready',
              last_message: '',
              last_message_role: 'agent',
              last_activity: event.payload.timestamp,
              created_at: event.payload.timestamp,
              closed_at: null,
            }];
          });
          break;

        case 'agent_close':
          setAgents(prev => prev.filter(a => a.session_id !== event.payload.session_id));
          break;

        case 'message':
          setAgents(prev => prev.map(a => {
            if (a.session_id !== event.payload.session_id) return a;
            return {
              ...a,
              last_message: event.payload.content,
              last_message_role: event.payload.role,
              last_activity: event.payload.timestamp,
            };
          }));
          break;

        case 'status':
          setAgents(prev => prev.map(a => {
            if (a.session_id !== event.payload.session_id) return a;
            return {
              ...a,
              status: event.payload.status,
              last_activity: event.payload.timestamp,
            };
          }));
          break;
      }
    });

    return unsubscribe;
  }, [fetchAgents]);

  return { agents, loading, error, refetch: fetchAgents };
}

// Group agents by project for display
export function groupByProject(agents: Agent[]): Map<string, Agent[]> {
  const groups = new Map<string, Agent[]>();
  
  agents.forEach(agent => {
    const project = agent.project || 'unknown';
    if (!groups.has(project)) {
      groups.set(project, []);
    }
    groups.get(project)!.push(agent);
  });

  return groups;
}
