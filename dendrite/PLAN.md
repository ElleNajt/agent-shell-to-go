# Dendrite - Agent Visualization App

## Vision

A mobile app that visualizes the agent topology as a navigable graph:
- Center: meta-agent conversation
- Nodes radiating out: project dispatchers
- Child nodes: individual agents per project
- Each node shows a status blurb (last message preview)
- Tap a node to chat with that agent
- Watch mode: observe streams without interacting

## Architecture

```
Emacs (agent-shell-to-go.el)
         |
         | HTTP POST (fire-and-forget)
         v
+------------------+
|  Backend Server  |  (lightweight Go)
|  - REST API      |  
|  - WebSocket     |  (for real-time updates to app)
|  - SQLite        |  (message history)
+------------------+
         |
         | WebSocket
         v
+------------------+
|  Mobile App      |  (React Native / Expo)
|  - Agents list   |
|  - Chat view     |
|  - Voice (later) |
+------------------+
```

## Implementation Status

### Phase 1: Backend Skeleton - COMPLETE
- [x] Create Go server with basic structure
- [x] SQLite schema for agents/messages
- [x] All endpoints implemented and tested
- [x] WebSocket broadcast setup
- [x] Bearer token authentication
- [x] Tailscale IP binding enforcement

### Phase 2: Emacs Integration - NOT STARTED
- [ ] Add HTTP posting to agent-shell-to-go.el
- [ ] Hook into existing events (spawn, close, message, status)
- [ ] Make it opt-in via defcustom (backend URL + token)

### Phase 3: Mobile App - Agents List - COMPLETE
- [x] React Native project setup (Expo)
- [x] Fetch /agents, display as grouped list
- [x] Status indicators per agent
- [x] Tap to select agent

### Phase 4: Mobile App - Chat View - COMPLETE
- [x] Chat UI for selected agent
- [x] Load history from /agents/:id/messages
- [x] Send messages via POST
- [x] Real-time updates via WebSocket
- [x] Keyboard handling

### Phase 5: Polish - PARTIAL
- [x] Dark mode (default)
- [ ] Reconnection handling (basic auto-reconnect exists)
- [ ] Offline indicator
- [ ] Pull-to-refresh (exists on agents list)
- [ ] Loading states

### Phase 6: Voice (future)
- [ ] Speech-to-text input
- [ ] Text-to-speech for agent responses
- [ ] Push-to-talk UI

## Events from Emacs

Endpoints agent-shell-to-go will POST to:

```
POST /events/agent-spawn
{
  "session_id": "abc123",
  "buffer_name": "Claude Code Agent @ myproject",
  "project": "myproject",
  "parent_session_id": null,  // or dispatcher's ID if spawned by one
  "timestamp": "2026-02-06T12:00:00Z"
}

POST /events/agent-close
{
  "session_id": "abc123",
  "timestamp": "2026-02-06T12:05:00Z"
}

POST /events/message
{
  "session_id": "abc123",
  "role": "user" | "agent" | "tool",
  "content": "...",
  "timestamp": "2026-02-06T12:01:00Z"
}

POST /events/status
{
  "session_id": "abc123",
  "status": "processing" | "ready" | "permission_required",
  "detail": "Running tests...",  // optional
  "timestamp": "..."
}
```

## API for Mobile App

```
GET /agents
Returns the current agent topology:
{
  "agents": [
    {
      "session_id": "abc123",
      "buffer_name": "Claude Code Agent @ myproject",
      "project": "myproject",
      "parent_session_id": null,
      "status": "ready",
      "last_message": "Tests passed.",
      "last_message_role": "agent",
      "last_activity": "2026-02-06T12:05:00Z"
    },
    ...
  ]
}

GET /agents/:session_id/messages?limit=50&before=...
Returns message history for an agent.

POST /agents/:session_id/send
{
  "content": "Run the tests"
}
Sends a message to the agent (forwarded to Emacs via WebSocket broadcast).

WebSocket /ws?token=...
Real-time stream of all events (same format as POST events).
Client filters by session_id for the agent they're viewing.
```

## Running the Backend

```bash
# Build
cd dendrite/backend
nix-shell -p go --run "go build -o dendrite-backend ."

# Create token
echo "your-secret-token" > ~/.dendrite-token

# Run (use your Tailscale IP)
./dendrite-backend \
  --listen 100.x.x.x:8080 \
  --token-file ~/.dendrite-token \
  --db ~/.agent-shell/dendrite.db
```

## Running the Mobile App

```bash
cd dendrite/app
npm install  # if needed
npx expo start
# Then press 'a' for Android or scan QR with Expo Go
```

## File Structure

```
dendrite/
  backend/
    main.go          # Complete Go server
    go.mod
  app/
    App.tsx          # Main app with navigation
    app.json         # Expo config
    package.json
    src/
      api/
        client.ts    # API client + WebSocket
      components/
        AgentNode.tsx  # Agent list item
      hooks/
        useAgents.ts   # Agents state + real-time updates
        useMessages.ts # Messages state for chat
      screens/
        SettingsScreen.tsx  # Backend URL/token config
        AgentsScreen.tsx    # Agent list grouped by project
        ChatScreen.tsx      # Chat with selected agent
```

## Next Steps

1. **Emacs Integration**: Add HTTP posting to agent-shell-to-go.el so events flow to the backend
2. **Test End-to-End**: Run backend, start an agent, see it appear in mobile app
3. **Graph View**: Optional - current list view may be sufficient

## Open Questions

1. **Receiving messages from mobile**: The `/agents/:id/send` endpoint broadcasts a `send_request` event. Emacs would need to listen on the WebSocket to receive these. Alternative: have Emacs poll an endpoint, or use a separate mechanism.

2. **Session IDs**: Currently using buffer names as implicit IDs. May need explicit UUIDs for robustness.

3. **Multiple machines**: Both Mac and fugue can POST to the same backend - it just aggregates everything.
