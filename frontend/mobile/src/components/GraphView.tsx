import React, { useMemo, useState, useRef } from 'react';
import { 
  View, 
  Text, 
  TouchableOpacity, 
  StyleSheet, 
  Dimensions,
  ScrollView,
  PanResponder,
} from 'react-native';
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
  const { width } = Dimensions.get('window');
  const [offset, setOffset] = useState({ x: 0, y: 0 });
  const offsetRef = useRef({ x: 0, y: 0 });

  const panResponder = useRef(
    PanResponder.create({
      onStartShouldSetPanResponder: () => false,
      onMoveShouldSetPanResponder: (_, gestureState) => {
        // Only pan if moving more than 5px (to allow taps)
        return Math.abs(gestureState.dx) > 5 || Math.abs(gestureState.dy) > 5;
      },
      onPanResponderMove: (_, gestureState) => {
        setOffset({
          x: offsetRef.current.x + gestureState.dx,
          y: offsetRef.current.y + gestureState.dy,
        });
      },
      onPanResponderRelease: (_, gestureState) => {
        offsetRef.current = {
          x: offsetRef.current.x + gestureState.dx,
          y: offsetRef.current.y + gestureState.dy,
        };
      },
    })
  ).current;

  // Build tree structure and calculate positions
  const { nodes, edges, bounds } = useMemo(() => {
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

    let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;

    // Layout tree with levels
    const layoutNode = (agent: Agent, x: number, y: number, level: number) => {
      const children = childrenOf.get(agent.session_id) || [];
      
      nodeMap.set(agent.session_id, {
        x,
        y,
        agent,
        children: children.map(c => c.session_id),
      });

      minX = Math.min(minX, x);
      maxX = Math.max(maxX, x);
      minY = Math.min(minY, y);
      maxY = Math.max(maxY, y);

      // Add edges to children
      children.forEach(child => {
        edgeList.push({ from: agent.session_id, to: child.session_id });
      });

      // Layout children with wider spacing
      const childSpacing = 140;
      const startX = x - (children.length - 1) * childSpacing / 2;

      children.forEach((child, i) => {
        layoutNode(child, startX + i * childSpacing, y + 120, level + 1);
      });
    };

    // Layout each root with wide spacing
    const rootSpacing = 180;
    const startX = width / 2 - (roots.length - 1) * rootSpacing / 2;

    roots.forEach((root, i) => {
      layoutNode(root, startX + i * rootSpacing, 100, 0);
    });

    return { 
      nodes: Array.from(nodeMap.values()), 
      edges: edgeList,
      bounds: { minX, maxX, minY, maxY },
    };
  }, [agents, width]);

  // Get node position by ID
  const getNodePos = (id: string) => {
    const node = nodes.find(n => n.agent.session_id === id);
    return node ? { x: node.x, y: node.y } : null;
  };

  // Calculate content size for scrolling
  const contentWidth = Math.max(bounds.maxX - bounds.minX + 200, width);
  const contentHeight = bounds.maxY - bounds.minY + 300;

  return (
    <View style={styles.container} {...panResponder.panHandlers}>
      <View 
        style={[
          styles.content,
          {
            transform: [
              { translateX: offset.x },
              { translateY: offset.y },
            ],
          },
        ]}
      >
        {/* Draw edges first (behind nodes) */}
        <View style={StyleSheet.absoluteFill} pointerEvents="none">
          {edges.map((edge, i) => {
            const from = getNodePos(edge.from);
            const to = getNodePos(edge.to);
            if (!from || !to) return null;

            // Draw vertical then horizontal lines (L-shaped connector)
            const fromY = from.y + 60; // Bottom of from node
            const toY = to.y - 5; // Top of to node
            const midY = fromY + (toY - fromY) / 2;

            return (
              <View key={`edge-${edge.from}-${edge.to}`}>
                {/* Vertical line from parent */}
                <View
                  style={[
                    styles.edge,
                    {
                      left: from.x,
                      top: fromY,
                      width: 2,
                      height: midY - fromY,
                    },
                  ]}
                />
                {/* Horizontal line */}
                <View
                  style={[
                    styles.edge,
                    {
                      left: Math.min(from.x, to.x),
                      top: midY,
                      width: Math.abs(to.x - from.x) + 2,
                      height: 2,
                    },
                  ]}
                />
                {/* Vertical line to child */}
                <View
                  style={[
                    styles.edge,
                    {
                      left: to.x,
                      top: midY,
                      width: 2,
                      height: toY - midY,
                    },
                  ]}
                />
              </View>
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
                  left: node.x - 55,
                  top: node.y,
                  borderColor: isSelected ? '#007AFF' : statusColor,
                  backgroundColor: isSelected ? '#1a3a5c' : '#1E1E1E',
                },
              ]}
              onPress={() => onSelectAgent(node.agent)}
            >
              <View style={[styles.statusDot, { backgroundColor: statusColor }]} />
              <Text style={styles.nodeName} numberOfLines={1}>{shortName}</Text>
              <Text style={styles.nodeProject} numberOfLines={1}>
                {node.agent.project.split('/').pop()}
              </Text>
            </TouchableOpacity>
          );
        })}
      </View>

      {agents.length === 0 && (
        <View style={styles.empty}>
          <Text style={styles.emptyText}>No agents running</Text>
          <Text style={styles.emptyHint}>Start an agent in Emacs to see it here</Text>
        </View>
      )}

      {agents.length > 0 && (
        <Text style={styles.hint}>Drag to pan</Text>
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: '#121212',
    overflow: 'hidden',
  },
  content: {
    flex: 1,
  },
  edge: {
    position: 'absolute',
    height: 2,
    backgroundColor: '#444',
  },
  node: {
    position: 'absolute',
    width: 110,
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
    fontSize: 11,
    fontWeight: '600',
    textAlign: 'center',
  },
  nodeProject: {
    color: '#888888',
    fontSize: 9,
    marginTop: 2,
  },
  empty: {
    ...StyleSheet.absoluteFillObject,
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
  hint: {
    position: 'absolute',
    bottom: 10,
    alignSelf: 'center',
    color: '#555',
    fontSize: 12,
  },
});
