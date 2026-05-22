# Discord Bot Setup

Steps to create a Discord bot for `agent-shell-to-go`.

## 1. Create the Application

1. Go to https://discord.com/developers/applications
2. Click **New Application**, give it a name (e.g. `agent-shell`), and confirm.
3. Note the **Application ID** on the General Information page — this is your app/bot user ID.

## 2. Configure the Bot

1. In the left sidebar, go to **Bot**.
2. Click **Reset Token**, copy and save the token — this is `agent-shell-to-go-discord-bot-token` (prefix with `Bot `).
3. Under **Privileged Gateway Intents**, enable:
   - **Server Members Intent** — needed to resolve user IDs
   - **Message Content Intent** — required to read message text (without this, `content` is always empty)

## 3. Set OAuth2 Scopes and Bot Permissions

1. In the sidebar, go to **OAuth2 → URL Generator**.
2. Under **Scopes**, check:
   - `bot`
   - `applications.commands`
3. Under **Bot Permissions**, check:

| Permission | Why |
|---|---|
| Read Messages / View Channels | See channels and threads |
| Send Messages | Post session output |
| Read Message History | Fetch prior messages |
| Manage Messages | Edit / delete bot messages |
| Manage Threads | Archive or delete forum posts |
| Manage Channels | Create forum channels per project |
| Add Reactions | React to messages |
| Attach Files | Upload file diffs |

4. Copy the generated URL, open it in a browser, and invite the bot to your server.

## 4. Create a Forum Channel

Forum channels (type 15) are required — one per project (or one shared default).

1. In your Discord server, create a new channel.
2. Set the channel type to **Forum**.
3. Copy the channel ID (right-click → **Copy Channel ID** with Developer Mode on).
4. Set `agent-shell-to-go-discord-channel-id` to this ID, or let `agent-shell-to-go` auto-create per-project forums if `agent-shell-to-go-discord-per-project-channels` is non-nil (requires `agent-shell-to-go-discord-guild-id`).

## 5. Get the Guild ID

1. Enable Developer Mode: Discord Settings → Advanced → Developer Mode.
2. Right-click your server icon → **Copy Server ID**.
3. Set `agent-shell-to-go-discord-guild-id` to this value.

## 6. Register Slash Commands

Run once after setup (or after changing command definitions):

```
M-x agent-shell-to-go-discord-register-commands
```

Guild-scoped commands (using `agent-shell-to-go-discord-guild-id`) are active immediately. Global commands can take up to an hour to propagate.

Then [configure credentials](../README.md#discord).
