import React, { useMemo } from 'react';
import { View, Text, TouchableOpacity, StyleSheet, Dimensions } from 'react-native';
import { Agent } from '../api/client';

interface GraphViewProps {
  agents: Agent[];
  selectedAgent: Agent | null;
  onSelectAgent: (agent: Agent) => void;
}

interface NodePosition {
  x: number;
  y: number;
  agent: Agent;
  children: string[];
}

const STATUS_COLORS: Record<string, string> = {
  ready: '#4CAF50',
  processing: '#FF9800',
  permission_required: '#F44336',
  closed: '#9E9E9E',
};

export function GraphView({ agents, selectedAgent, onSelectAgent }: GraphViewProps) {
  const { width, height } = Dimensions.get('window');
  const centerX = width / 2;
  const centerY = height / 2 - 50;

  // Build tree structure and calculate positions
  const { nodes, edges } = useMemo(() => {
    const nodeMap = new Map<string, NodePosition>();
    const edgeList: { from: string; to: string }[] = [];

    // Find root nodes (no parent)
    const roots = agents.filter(a => !a.parent_session_id);
    const childrenOf = new Map<string, Agent[]>();

    // Group children by parent
    agents.forEach(agent => {
      if (agent.parent_session_id) {
        const siblings = childrenOf.get(agent.parent_session_id) || [];
        siblings.push(agent);
        childrenOf.set(agent.parent_session_id, siblings);
      }
    });

    // Layout tree with levels
    const layoutNode = (agent: Agent, x: number, y: number, level: number, siblingIndex: number, siblingCount: number) => {
      const children = childrenOf.get(agent.session_id) || [];
      
      nodeMap.set(agent.session_id, {
        x,
        y,
        agent,
        children: children.map(c => c.session_id),
      });

      // Add edges to children
      children.forEach(child => {
        edgeList.push({ from: agent.session_id, to: child.session_id });
      });

      // Layout children
      const childSpacing = Math.min(160, (width - 40) / Math.max(children.length, 1));
      const startX = x - (children.length - 1) * childSpacing / 2;

      children.forEach((child, i) => {
        layoutNode(child, startX + i * childSpacing, y + 140, level + 1, i, children.length);
      });
    };

    // Layout each root
    const rootSpacing = Math.min(200, (width - 40) / Math.max(roots.length, 1));
    const startX = centerX - (roots.length - 1) * rootSpacing / 2;

    roots.forEach((root, i) => {
      layoutNode(root, startX + i * rootSpacing, 80, 0, i, roots.length);
    });

    return { nodes: Array.from(nodeMap.values()), edges: edgeList };
  }, [agents, width, centerX]);

  // Get node position by ID
  const getNodePos = (id: string) => {
    const node = nodes.find(n => n.agent.session_id === id);
    return node ? { x: node.x, y: node.y } : null;
  };

  return (
    <View style={styles.container}>
      {/* Draw edges first (behind nodes) */}
      <View style={StyleSheet.absoluteFill}>
        {edges.map((edge, i) => {
          const from = getNodePos(edge.from);
          const to = getNodePos(edge.to);
          if (!from || !to) return null;

          // Draw a simple line using a rotated view
          const dx = to.x - from.x;
          const dy = to.y - from.y;
          const length = Math.sqrt(dx * dx + dy * dy);
          const angle = Math.atan2(dy, dx) * 180 / Math.PI;

          return (
            <View
              key={i}
              style={[
                styles.edge,
                {
                  left: from.x,
                  top: from.y + 30,
                  width: length,
                  transform: [{ rotate: `${angle}deg` }],
                  transformOrigin: 'left center',
                },
              ]}
            />
          );
        })}
      </View>

      {/* Draw nodes */}
      {nodes.map(node => {
        const isSelected = selectedAgent?.session_id === node.agent.session_id;
        const statusColor = STATUS_COLORS[node.agent.status] || STATUS_COLORS.ready;
        const name = node.agent.buffer_name.split(' @ ')[0] || node.agent.buffer_name;
        const shortName = name.length > 15 ? name.slice(0, 12) + '...' : name;

        return (
          <TouchableOpacity
            key={node.agent.session_id}
            style={[
              styles.node,
              {
                left: node.x - 50,
                top: node.y,
                borderColor: isSelected ? '#007AFF' : statusColor,
                backgroundColor: isSelected ? '#1a3a5c' : '#1E1E1E',
              },
            ]}
            onPress={() => onSelectAgent(node.agent)}
          >
            <View style={[styles.statusDot, { backgroundColor: statusColor }]} />
            <Text style={styles.nodeName} numberOfLines={1}>{shortName}</Text>
            <Text style={styles.nodeProject}>{node.agent.project}</Text>
            {node.agent.last_message ? (
              <Text style={styles.nodePreview} numberOfLines={1}>
                {node.agent.last_message.slice(0, 30)}...
              </Text>
            ) : null}
          </TouchableOpacity>
        );
      })}

      {agents.length === 0 && (
        <View style={styles.empty}>
          <Text style={styles.emptyText}>No agents running</Text>
          <Text style={styles.emptyHint}>Start an agent in Emacs to see it here</Text>
        </View>
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
  },
  edge: {
    position: 'absolute',
    height: 2,
    backgroundColor: '#444',
  },
  node: {
    position: 'absolute',
    width: 100,
    padding: 10,
    borderRadius: 12,
    borderWidth: 2,
    alignItems: 'center',
  },
  statusDot: {
    width: 10,
    height: 10,
    borderRadius: 5,
    marginBottom: 6,
  },
  nodeName: {
    color: '#FFFFFF',
    fontSize: 12,
    fontWeight: '600',
    textAlign: 'center',
  },
  nodeProject: {
    color: '#888888',
    fontSize: 10,
    marginTop: 2,
  },
  nodePreview: {
    color: '#666666',
    fontSize: 9,
    marginTop: 4,
    textAlign: 'center',
  },
  empty: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  emptyText: {
    color: '#FFFFFF',
    fontSize: 18,
    fontWeight: '600',
  },
  emptyHint: {
    color: '#888888',
    fontSize: 14,
    marginTop: 8,
  },
});
