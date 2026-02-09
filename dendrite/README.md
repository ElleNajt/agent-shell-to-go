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

## Setup

### 1. Emacs Configuration

Add to your Doom Emacs config (inside the `use-package! agent-shell-to-go` block):

```elisp
(require 'agent-shell-to-go-mobile)
(setq agent-shell-to-go-mobile-backend-url
      (format "http://%s:8080"
              (string-trim (shell-command-to-string "tailscale ip -4"))))
(agent-shell-to-go-mobile-setup)
```

This auto-detects the machine's Tailscale IP at startup and configures Emacs to push agent events to the backend.

### 2. Start the Backend

```bash
cd dendrite/backend
./start-backend.sh
```

This will:
- Build the Go binary if needed (or pass `--build` to force rebuild)
- Auto-detect your Tailscale IP
- Attempt to configure Emacs via emacsclient (optional — the Emacs config above handles this persistently)
- Start the backend on `<tailscale-ip>:8080`

### 3. Mobile App

In the app, add a machine with the backend URL (e.g., `http://100.x.x.x:8080`). You can switch between machines by tapping the machine name in the header.

For development:
```bash
cd app
npm install
npx expo start
```

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

## Security

- Backend only listens on Tailscale IPs (100.x.x.x)
- Tailscale is the auth layer — no application-level tokens
- Only devices on your tailnet can reach the backend

### Isolating Untrusted Machines

If you run agents on a machine you don't fully trust (e.g., a cloud GPU box), a prompt-injected agent could potentially reach other machines' backends and send commands. To prevent this, use Tailscale ACLs to restrict which machines can talk to which.

**The threat model:** An attacker gets malicious text into an agent's context (via a file, web page, tool output, etc.). The agent gets prompt-injected and tries to `curl` another machine's backend to take it over.

**The fix:** Configure Tailscale ACLs so untrusted machines can only reach themselves, not other backends.

#### Setting up ACLs

1. Go to https://login.tailscale.com/admin/acls
2. Find your tailnet domain by running: `tailscale status --json | grep DNSName`
   - It looks like `*.your-tailnet.ts.net`
3. Replace the default "allow all" ACL with something like:

```json
{
  "acls": [
    // Untrusted machine can only reach itself
    {
      "action": "accept",
      "src": ["untrusted-box.your-tailnet.ts.net"],
      "dst": ["untrusted-box.your-tailnet.ts.net:*"]
    },
    
    // Trusted machines can reach everything
    {
      "action": "accept",
      "src": ["macbook.your-tailnet.ts.net", "phone.your-tailnet.ts.net"],
      "dst": ["*:*"]
    }
  ],
  
  "tests": [
    // Untrusted can reach itself
    {
      "src": "untrusted-box.your-tailnet.ts.net",
      "accept": ["untrusted-box.your-tailnet.ts.net:8080"]
    },
    // Untrusted cannot reach other machines
    {
      "src": "untrusted-box.your-tailnet.ts.net",
      "deny": ["macbook.your-tailnet.ts.net:8080"]
    },
    // Trusted can reach untrusted (for SSH, monitoring, etc.)
    {
      "src": "macbook.your-tailnet.ts.net",
      "accept": ["untrusted-box.your-tailnet.ts.net:8080", "untrusted-box.your-tailnet.ts.net:22"]
    }
  ]
}
```

This ensures:
- Untrusted machine → itself: ✅
- Untrusted machine → other backends: ❌ (prompt injection can't pivot)
- Trusted machines → untrusted: ✅ (you can still SSH in and use the mobile app)
