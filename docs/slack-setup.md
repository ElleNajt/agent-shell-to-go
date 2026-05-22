# Slack Setup

Steps to create and configure the Slack app for `agent-shell-to-go`.

## Quick setup (recommended)

1. Go to https://api.slack.com/apps
2. Click "Create New App" → "From an app manifest"
3. Select your workspace
4. Paste the contents of [`slack-app-manifest.yaml`](slack-app-manifest.yaml)
5. Click "Create"
6. Go to "OAuth & Permissions" → "Install to Workspace" → copy the Bot Token (`xoxb-...`)
7. Go to "Basic Information" → "App-Level Tokens" → "Generate Token" with `connections:write` scope → copy it (`xapp-...`)
8. Get your channel ID (right-click channel → "View channel details" → scroll to bottom)
9. Invite the bot to your channel: `/invite @agent-shell-to-go`

Then [configure credentials](../README.md#credentials).

## Manual setup

<details>
<summary>Click to expand step-by-step guide</summary>

1. **Create the app**
   - Go to https://api.slack.com/apps
   - Click "Create New App" → "From scratch"
   - Name it something like "agent-shell-to-go"
   - Select your workspace

2. **Enable Socket Mode**
   - In the sidebar, click "Socket Mode"
   - Toggle "Enable Socket Mode" ON
   - When prompted, create an app-level token named "websocket" with the `connections:write` scope
   - **Save this token** (starts with `xapp-`)

3. **Add Bot Token Scopes**
   - In the sidebar, click "OAuth & Permissions" → "Scopes" → "Bot Token Scopes"
   - Add: `chat:write`, `channels:history`, `channels:read`, `reactions:read`

4. **Subscribe to Events**
   - In the sidebar, click "Event Subscriptions" → toggle "Enable Events" ON
   - Under "Subscribe to bot events", add:
     - `message.channels`
     - `reaction_added`
     - `reaction_removed`
   - Click "Save Changes"

5. **Add Slash Commands**
   - In the sidebar, click "Slash Commands" and create:
     - `/new-project` — "Create a new project and start an agent"
     - `/new-agent` — "Start new agent in a folder"

6. **Install the App**
   - Click "Install App" → "Install to Workspace" → "Allow"
   - Copy the **Bot User OAuth Token** (starts with `xoxb-`)

7. **Set up your channel**
   - Create or use an existing channel (e.g., `#agent-shell`)
   - Invite the bot: `/invite @your-bot-name`
   - Get the channel ID: right-click the channel → "View channel details" → scroll to bottom (starts with `C`)

</details>
