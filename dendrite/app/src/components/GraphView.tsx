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

    // Build a map of project -> dispatcher for fallback parenting
    const dispatcherByProject = new Map<string, Agent>();
    let metaAgent: Agent | null = null;
    
    agents.forEach(agent => {
      if (agent.buffer_name.toLowerCase().includes('dispatcher')) {
        dispatcherByProject.set(agent.project, agent);
      }
      // Find the meta agent (.claude-meta project)
      if (agent.project === '.claude-meta' || agent.project.endsWith('/.claude-meta')) {
        metaAgent = agent;
      }
    });

    // Infer parent relationships:
    // - Dispatchers -> meta agent (if exists)
    // - Other agents -> their project's dispatcher (if exists)
    const getEffectiveParent = (agent: Agent): string | null => {
      if (agent.parent_session_id) return agent.parent_session_id;
      
      // Meta agent is the root
      if (agent.project === '.claude-meta' || agent.project.endsWith('/.claude-meta')) {
        return null;
      }
      
      // Dispatchers are children of the meta agent
      if (agent.buffer_name.toLowerCase().includes('dispatcher')) {
        return metaAgent ? metaAgent.session_id : null;
      }
      
      // Other agents are children of their project's dispatcher
      const dispatcher = dispatcherByProject.get(agent.project);
      return dispatcher ? dispatcher.session_id : null;
    };

    // Categorize agents:
    // - True roots: meta agent or agents with no possible parent
    // - Dispatchers: go to ring 1
    // - Orphaned agents: non-dispatchers with no parent/dispatcher, go to ring 2
    const childrenOf = new Map<string, Agent[]>();
    const trueRoots: Agent[] = [];
    const orphanedAgents: Agent[] = [];

    // Group children by effective parent, identify roots and orphans
    agents.forEach(agent => {
      const parentId = getEffectiveParent(agent);
      if (parentId) {
        const siblings = childrenOf.get(parentId) || [];
        siblings.push(agent);
        childrenOf.set(parentId, siblings);
      } else {
        // No parent - is this a true root or an orphan?
        const isMetaAgent = agent.project === '.claude-meta' || agent.project.endsWith('/.claude-meta');
        const isDispatcher = agent.buffer_name.toLowerCase().includes('dispatcher');
        
        if (isMetaAgent) {
          // Meta agent is always a true root
          trueRoots.push(agent);
        } else if (isDispatcher) {
          // Dispatcher without meta agent - true root at ring 1 level
          trueRoots.push(agent);
        } else {
          // Regular agent with no parent/dispatcher - orphan, goes to ring 2
          orphanedAgents.push(agent);
        }
      }
    });

    // Ring-based layout: collect nodes by depth, then distribute each ring evenly
    // This prevents overlap between nodes from different subtrees
    // Ring 0: meta agent (center)
    // Ring 1: dispatchers
    // Ring 2: worker agents (including orphans)
    
    // Step 1: Assign depth to each node via BFS
    const depthOf = new Map<string, number>();
    const nodesByDepth: Agent[][] = [];
    
    const assignDepths = (agent: Agent, depth: number) => {
      depthOf.set(agent.session_id, depth);
      if (!nodesByDepth[depth]) nodesByDepth[depth] = [];
      nodesByDepth[depth].push(agent);
      
      const children = childrenOf.get(agent.session_id) || [];
      children.forEach(child => assignDepths(child, depth + 1));
    };
    
    trueRoots.forEach(root => assignDepths(root, 0));
    
    // Add orphaned agents to ring 2 (or create it if needed)
    if (orphanedAgents.length > 0) {
      if (!nodesByDepth[2]) nodesByDepth[2] = [];
      orphanedAgents.forEach(agent => {
        depthOf.set(agent.session_id, 2);
        nodesByDepth[2].push(agent);
      });
    }
    
    // Step 2: Calculate radius for each ring based on node count
    // Ensure minimum spacing of 280px between nodes on each ring
    const minSpacing = 280;
    const ringRadii: number[] = [];
    let baseRadius = 0; // Ring 0 starts at center if single root
    
    nodesByDepth.forEach((nodesAtDepth, depth) => {
      if (depth === 0 && nodesAtDepth.length === 1) {
        // Single root at center
        ringRadii[depth] = 0;
        baseRadius = 200; // Next ring starts here
      } else {
        // Calculate radius needed for this many nodes with minSpacing
        const circumference = nodesAtDepth.length * minSpacing;
        const neededRadius = circumference / (2 * Math.PI);
        // Ensure rings don't shrink and maintain minimum gap between rings
        const minRadius = baseRadius + (depth === 0 ? 0 : 160);
        ringRadii[depth] = Math.max(neededRadius, minRadius);
        baseRadius = ringRadii[depth];
      }
    });
    
    // Step 3: Position ring 0 and ring 1 first (roots and dispatchers)
    // Then position ring 2+ agents near their parent/project dispatcher
    
    // Track dispatcher angles by project for positioning children nearby
    const dispatcherAngles = new Map<string, number>();
    
    // Position ring 0 (meta agent / roots)
    if (nodesByDepth[0]) {
      const radius = ringRadii[0];
      nodesByDepth[0].forEach((agent, i) => {
        let x: number, y: number, angle: number;
        
        if (radius === 0) {
          x = centerX;
          y = centerY;
          angle = 0;
        } else {
          angle = (2 * Math.PI * i) / nodesByDepth[0].length - Math.PI / 2;
          x = centerX + Math.cos(angle) * radius;
          y = centerY + Math.sin(angle) * radius;
        }
        
        const children = childrenOf.get(agent.session_id) || [];
        nodeMap.set(agent.session_id, { x, y, agent, children: children.map(c => c.session_id) });
        children.forEach(child => edgeList.push({ from: agent.session_id, to: child.session_id }));
      });
    }
    
    // Position ring 1 (dispatchers) and record their angles - sorted alphabetically
    if (nodesByDepth[1]) {
      const radius = ringRadii[1];
      const sortedDispatchers = [...nodesByDepth[1]].sort((a, b) => 
        a.buffer_name.localeCompare(b.buffer_name)
      );
      sortedDispatchers.forEach((agent, i) => {
        const angle = (2 * Math.PI * i) / sortedDispatchers.length - Math.PI / 2;
        const x = centerX + Math.cos(angle) * radius;
        const y = centerY + Math.sin(angle) * radius;
        
        // Record angle for this project's dispatcher
        dispatcherAngles.set(agent.project, angle);
        
        const children = childrenOf.get(agent.session_id) || [];
        nodeMap.set(agent.session_id, { x, y, agent, children: children.map(c => c.session_id) });
        children.forEach(child => edgeList.push({ from: agent.session_id, to: child.session_id }));
      });
    }
    
    // Position ring 2+ agents grouped by project, near their dispatcher
    for (let depth = 2; depth < nodesByDepth.length; depth++) {
      if (!nodesByDepth[depth]) continue;
      const radius = ringRadii[depth];
      
      // Sort agents by project, then alphabetically by buffer name
      const sortedAgents = [...nodesByDepth[depth]].sort((a, b) => {
        const projectCmp = a.project.localeCompare(b.project);
        if (projectCmp !== 0) return projectCmp;
        return a.buffer_name.localeCompare(b.buffer_name);
      });
      
      // Group by project
      const byProject = new Map<string, Agent[]>();
      sortedAgents.forEach(agent => {
        const list = byProject.get(agent.project) || [];
        list.push(agent);
        byProject.set(agent.project, list);
      });
      
      // Position each project's agents near their dispatcher
      const projects = Array.from(byProject.keys());
      const totalAgents = sortedAgents.length;
      let agentIndex = 0;
      
      projects.forEach(project => {
        const projectAgents = byProject.get(project)!;
        const dispatcherAngle = dispatcherAngles.get(project);
        
        projectAgents.forEach((agent, i) => {
          let angle: number;
          
          if (dispatcherAngle !== undefined) {
            // Position near dispatcher, spreading agents around it
            // Use ~0.3 radians (~17 degrees) between agents for adequate spacing
            const angleStep = 0.3;
            const offset = i - (projectAgents.length - 1) / 2;
            angle = dispatcherAngle + offset * angleStep;
          } else {
            // No dispatcher for this project - distribute evenly in remaining space
            angle = (2 * Math.PI * agentIndex) / totalAgents - Math.PI / 2;
          }
          
          const x = centerX + Math.cos(angle) * radius;
          const y = centerY + Math.sin(angle) * radius;
          
          const children = childrenOf.get(agent.session_id) || [];
          nodeMap.set(agent.session_id, { x, y, agent, children: children.map(c => c.session_id) });
          children.forEach(child => edgeList.push({ from: agent.session_id, to: child.session_id }));
          
          agentIndex++;
        });
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
          const isDispatcher = node.agent.buffer_name.toLowerCase().includes('dispatcher');
          const name = node.agent.buffer_name.split(' @ ')[0] || node.agent.buffer_name;
          const projectName = node.agent.project.split('/').pop() || node.agent.project;

          return (
            <TouchableOpacity
              key={node.agent.session_id}
              style={[
                styles.node,
                {
                  left: node.x,
                  top: node.y,
                  borderColor: isSelected ? '#007AFF' : statusColor,
                  backgroundColor: isSelected ? '#1a3a5c' : '#1E1E1E',
                },
              ]}
              onPress={() => onSelectAgent(node.agent)}
            >
              {isDispatcher ? (
                <>
                  <Text style={styles.dispatcherHat}>🎩</Text>
                  <Text style={styles.dispatcherProject}>{projectName}</Text>
                  <View style={[styles.statusDot, { backgroundColor: statusColor, marginTop: 4 }]} />
                </>
              ) : (
                <>
                  <View style={[styles.statusDot, { backgroundColor: statusColor }]} />
                  <Text style={styles.nodeName}>{name}</Text>
                  <Text style={styles.nodeProject}>{projectName}</Text>
                </>
              )}
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
    minWidth: 80,
    maxWidth: 180,
    paddingHorizontal: 12,
    paddingVertical: 10,
    borderRadius: 12,
    borderWidth: 2,
    alignItems: 'center',
    transform: [{ translateX: '-50%' }, { translateY: '-50%' }],
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
  dispatcherHat: {
    fontSize: 18,
    marginBottom: 2,
  },
  dispatcherProject: {
    color: '#FFFFFF',
    fontSize: 13,
    fontWeight: '700',
    textAlign: 'center',
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
