#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CONFIG_DIR="$HOME/.dendrite"
CONFIG_FILE="$CONFIG_DIR/config.json"

# Cleanup function
cleanup() {
    echo ""
    echo "Shutting down..."
    if [ -n "$BACKEND_PID" ]; then
        kill $BACKEND_PID 2>/dev/null
        echo "Backend stopped"
    fi
    exit 0
}

# Set trap before starting anything
trap cleanup INT TERM EXIT

# Check for config file
if [ ! -f "$CONFIG_FILE" ]; then
    echo "Config not found at $CONFIG_FILE"
    echo "Creating default config..."
    mkdir -p "$CONFIG_DIR"

    # Get Tailscale IP for default config
    TAILSCALE_IP=$(/Applications/Tailscale.app/Contents/MacOS/Tailscale ip -4 2>/dev/null || ifconfig | grep "inet 100\." | awk '{print $2}')

    cat >"$CONFIG_FILE" <<EOF
{
  "machines": [
    { "name": "$(hostname -s)", "url": "http://${TAILSCALE_IP:-YOUR_IP}:8080" }
  ]
}
EOF
    echo "Created $CONFIG_FILE - edit it to add your machines"
fi

# Copy config to app directory
cp "$CONFIG_FILE" "$SCRIPT_DIR/dendrite/app/config.json"
echo "Using config from $CONFIG_FILE"

# Get listen IP from config or detect it
TAILSCALE_IP=$(/Applications/Tailscale.app/Contents/MacOS/Tailscale ip -4 2>/dev/null || ifconfig | grep "inet 100\." | awk '{print $2}')

# Start backend
echo "Starting backend on $TAILSCALE_IP:8080..."
cd "$SCRIPT_DIR/dendrite/backend"
nix --extra-experimental-features 'nix-command flakes' shell nixpkgs#go -c go build -o dendrite-backend main.go
AGENT_SHELL_API_TOKEN=NOAUTH nix --extra-experimental-features 'nix-command flakes' shell nixpkgs#go -c ./dendrite-backend --listen "$TAILSCALE_IP:8080" &
BACKEND_PID=$!
echo "Backend started (PID: $BACKEND_PID)"

# Wait for backend to be ready
sleep 2
if curl -s "http://$TAILSCALE_IP:8080/health" >/dev/null; then
    echo "Backend is healthy"
else
    echo "Warning: Backend health check failed"
fi

# Start mobile app
echo "Starting Expo..."
cd "$SCRIPT_DIR/dendrite/app"
npx expo start
