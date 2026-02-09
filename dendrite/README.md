# Dendrite

Mobile app and backend for monitoring and controlling agent-shell sessions across multiple machines.

## Architecture

```
Mobile App (React Native)
    │
    ├── WebSocket ──► Backend (machine 1) ◄── Emacs (machine 1)
    │
    └── WebSocket ──► Backend (machine 2) ◄── Emacs (machine 2)
```

Each machine runs its own backend. The mobile app can switch between machines.

## Quick Start (Single Machine)

```bash
cd dendrite
./start.sh
```

This starts the backend and Expo dev server. Scan the QR code with Expo Go.

## Multi-Machine Setup

### On each machine you want to monitor:

1. **Start the backend:**
```bash
cd dendrite/backend
./start-backend.sh
```

This will:
- Auto-detect your Tailscale IP
- Create a token file at `~/.agent-shell-api-token` if needed
- Configure Emacs automatically via emacsclient

### In the mobile app:

1. Tap the machine name in the header (top left)
2. Tap "Add Machine"
3. Enter a name and the backend URL (e.g., `http://100.x.x.x:8080`)

You can switch between machines by tapping the machine name.

## Components

### Backend (`backend/`)

Go server that:
- Receives events from Emacs (agent spawn, messages, status changes)
- Stores session data in SQLite
- Broadcasts updates to mobile app via WebSocket
- Forwards commands from mobile app to Emacs

### Mobile App (`app/`)

React Native app that:
- Displays agent sessions in a graph view
- Shows real-time message streams
- Allows sending messages to agents
- Can stop/close agents remotely

## Development

### Backend
```bash
cd backend
go build -o dendrite-backend .
./dendrite-backend --listen 127.0.0.1:8080 --allow-localhost
```

### Mobile App
```bash
cd app
npm install
npx expo start
```

## Security

- Backend only listens on Tailscale IPs (100.x.x.x) by default
- All requests require Bearer token authentication
- Token is stored in `~/.agent-shell-api-token`
