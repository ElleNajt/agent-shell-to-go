import React, { useMemo, useState, useRef } from 'react';
import { 
  View, 
  Text, 
  TouchableOpacity, 
  StyleSheet, 
  Dimensions,
  PanResponder,
  GestureResponderEvent,
} from 'react-native';
import { Agent } from '../api/client';

// Calculate distance between two touch points
function getDistance(touches: { pageX: number; pageY: number }[]): number {
  const dx = touches[0].pageX - touches[1].pageX;
  const dy = touches[0].pageY - touches[1].pageY;
  return Math.sqrt(dx * dx + dy * dy);
}

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
  const centerY = height / 2 - 100;
  
  const [offset, setOffset] = useState({ x: 0, y: 0 });
  const [scale, setScale] = useState(1);
  const offsetRef = useRef({ x: 0, y: 0 });
  const scaleRef = useRef(1);
  const initialPinchDistance = useRef<number | null>(null);
  const isPinching = useRef(false);

  const panResponder = useRef(
    PanResponder.create({
      onStartShouldSetPanResponder: () => false,
      onMoveShouldSetPanResponder: (evt: GestureResponderEvent, gestureState) => {
        // Always capture if two fingers (pinch gesture)
        if (evt.nativeEvent.touches.length >= 2) {
          return true;
        }
        // Single finger pan
        return Math.abs(gestureState.dx) > 5 || Math.abs(gestureState.dy) > 5;
      },
      onPanResponderGrant: (evt: GestureResponderEvent) => {
        // Initialize pinch if starting with two fingers
        if (evt.nativeEvent.touches.length >= 2) {
          const touches = evt.nativeEvent.touches;
          initialPinchDistance.current = getDistance([
            { pageX: touches[0].pageX, pageY: touches[0].pageY },
            { pageX: touches[1].pageX, pageY: touches[1].pageY },
          ]);
          isPinching.current = true;
        }
      },
      onPanResponderMove: (evt: GestureResponderEvent, gestureState) => {
        const touches = evt.nativeEvent.touches;
        
        // Handle pinch zoom
        if (touches.length >= 2 && initialPinchDistance.current !== null) {
          isPinching.current = true;
          const currentDistance = getDistance([
            { pageX: touches[0].pageX, pageY: touches[0].pageY },
            { pageX: touches[1].pageX, pageY: touches[1].pageY },
          ]);
          const pinchScale = currentDistance / initialPinchDistance.current;
          const newScale = Math.max(0.3, Math.min(3, scaleRef.current * pinchScale));
          setScale(newScale);
          // Update initial distance for continuous pinching
          initialPinchDistance.current = currentDistance;
          scaleRef.current = newScale;
          return;
        }
        
        // Handle pan (only if not pinching)
        if (!isPinching.current) {
          setOffset({
            x: offsetRef.current.x + gestureState.dx,
            y: offsetRef.current.y + gestureState.dy,
          });
        }
      },
      onPanResponderRelease: (evt: GestureResponderEvent, gestureState) => {
        if (isPinching.current) {
          // Reset pinch state
          initialPinchDistance.current = null;
          isPinching.current = false;
        } else {
          // Commit pan change
          offsetRef.current = {
            x: offsetRef.current.x + gestureState.dx,
            y: offsetRef.current.y + gestureState.dy,
          };
        }
      },
    })
  ).current;

  // Build tree structure and calculate positions in a radial layout
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

    // Radial layout - place nodes in concentric circles
    const layoutRadial = (agent: Agent, cx: number, cy: number, angle: number, radius: number, arcSpan: number) => {
      const x = cx + Math.cos(angle) * radius;
      const y = cy + Math.sin(angle) * radius;
      
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

      // Layout children in an arc
      if (children.length > 0) {
        const childRadius = radius + 130;
        const childArcSpan = Math.min(arcSpan, Math.PI * 0.8); // Limit spread
        const startAngle = angle - childArcSpan / 2;
        const angleStep = children.length > 1 ? childArcSpan / (children.length - 1) : 0;

        children.forEach((child, i) => {
          const childAngle = children.length === 1 ? angle : startAngle + angleStep * i;
          layoutRadial(child, cx, cy, childAngle, childRadius, childArcSpan / children.length);
        });
      }
    };

    // Layout roots
    if (roots.length === 1) {
      // Single root at center
      const root = roots[0];
      const children = childrenOf.get(root.session_id) || [];
      
      nodeMap.set(root.session_id, {
        x: centerX,
        y: centerY,
        agent: root,
        children: children.map(c => c.session_id),
      });

      // Children spread around in a circle
      children.forEach((child, i) => {
        edgeList.push({ from: root.session_id, to: child.session_id });
        const angle = (2 * Math.PI * i) / children.length - Math.PI / 2; // Start from top
        layoutRadial(child, centerX, centerY, angle, 140, (2 * Math.PI) / children.length);
      });
    } else {
      // Multiple roots spread in a circle
      roots.forEach((root, i) => {
        const angle = (2 * Math.PI * i) / roots.length - Math.PI / 2;
        layoutRadial(root, centerX, centerY, angle, 0, (2 * Math.PI) / roots.length);
      });
    }

    return { nodes: Array.from(nodeMap.values()), edges: edgeList };
  }, [agents, centerX, centerY]);

  // Get node position by ID
  const getNodePos = (id: string) => {
    const node = nodes.find(n => n.agent.session_id === id);
    return node ? { x: node.x, y: node.y } : null;
  };

  return (
    <View style={styles.container} {...panResponder.panHandlers}>
      <View 
        style={[
          styles.content,
          {
            transform: [
              { translateX: offset.x },
              { translateY: offset.y },
              { scale },
            ],
          },
        ]}
      >
        {/* Draw edges as straight lines */}
        <View style={StyleSheet.absoluteFill} pointerEvents="none">
          {edges.map((edge) => {
            const from = getNodePos(edge.from);
            const to = getNodePos(edge.to);
            if (!from || !to) return null;

            const dx = to.x - from.x;
            const dy = to.y - from.y;
            const length = Math.sqrt(dx * dx + dy * dy);
            const angle = Math.atan2(dy, dx) * 180 / Math.PI;

            return (
              <View
                key={`edge-${edge.from}-${edge.to}`}
                style={[
                  styles.edge,
                  {
                    left: from.x,
                    top: from.y,
                    width: length,
                    transform: [
                      { translateX: 0 },
                      { translateY: -1 },
                      { rotate: `${angle}deg` },
                    ],
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
                  left: node.x - 55,
                  top: node.y - 30,
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
        <Text style={styles.hint}>Drag to pan, pinch to zoom</Text>
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
