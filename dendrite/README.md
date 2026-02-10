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

Each machine with agents runs its own backend. The mobile app connects to all of them and lets you switch between machines. You only need to run the app (Expo) on one machine - typically your laptop.

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

### 2. Configure Machines

Create `~/.dendrite/config.json` with your machines:

```json
{
  "machines": [
    { "name": "MacBook", "url": "http://100.x.x.x:8080" },
    { "name": "GPU Box", "url": "http://100.y.y.y:8080" }
  ]
}
```

Use your Tailscale IPs. You can find them with `tailscale ip -4`.

### 3. Start the Backend

On each machine where you run agents:

```bash
cd dendrite/backend
go build -o dendrite-backend main.go
AGENT_SHELL_API_TOKEN=NOAUTH ./dendrite-backend --listen "$(tailscale ip -4):8080"
```

### 4. Start the App (one machine only)

On your laptop (or wherever you want to run Expo):

```bash
./start.sh
```

This will:
- Copy your config to the app directory
- Build and start the local backend
- Start the Expo dev server for the mobile app

Or if you only want the app (backend already running elsewhere):

```bash
cp ~/.dendrite/config.json dendrite/app/config.json
cd dendrite/app
npx expo start
```

### 5. Mobile App

The app reads the machines from your config. You can switch between machines by tapping the machine name in the header.

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

Tailscale ACLs use tags to identify machines. You need to:
1. Define tags and their owners
2. Write ACL rules using those tags
3. Apply tags to your machines in the admin console

**Step 1:** Go to https://login.tailscale.com/admin/acls and replace the default ACL:

```json
{
  "tagOwners": {
    "tag:untrusted": ["autogroup:admin"],
    "tag:trusted": ["autogroup:admin"]
  },
  
  "acls": [
    // Untrusted machines can only reach themselves
    {
      "action": "accept",
      "src": ["tag:untrusted"],
      "dst": ["tag:untrusted:*"]
    },
    
    // Trusted machines can reach everything
    {
      "action": "accept",
      "src": ["tag:trusted"],
      "dst": ["*:*"]
    }
  ],
  
  "tests": [
    // Untrusted can reach other untrusted machines
    {
      "src": "tag:untrusted",
      "accept": ["tag:untrusted:22", "tag:untrusted:8080"]
    },
    // Untrusted cannot reach trusted machines
    {
      "src": "tag:untrusted",
      "deny": ["tag:trusted:8080", "tag:trusted:22"]
    },
    // Trusted can reach everything
    {
      "src": "tag:trusted",
      "accept": ["tag:untrusted:8080", "tag:untrusted:22"]
    }
  ]
}
```

**Step 2:** Apply tags to your machines at https://login.tailscale.com/admin/machines:
- Tag your cloud GPU boxes with `tag:untrusted`
- Tag your MacBook, phone, and other trusted devices with `tag:trusted`

This ensures:
- Untrusted machine → itself/other untrusted: ✅
- Untrusted machine → trusted backends: ❌ (prompt injection can't pivot)
- Trusted machines → everything: ✅ (you can still SSH in and use the mobile app)
