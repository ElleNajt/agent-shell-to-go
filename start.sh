#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Get Tailscale IP
TAILSCALE_IP=$(/Applications/Tailscale.app/Contents/MacOS/Tailscale ip -4 2>/dev/null || ifconfig | grep "inet 100\." | awk '{print $2}')

if [ -z "$TAILSCALE_IP" ]; then
    echo "Error: Could not find Tailscale IP"
    exit 1
fi

echo "Using Tailscale IP: $TAILSCALE_IP"

# Update mobile config
cat >"$SCRIPT_DIR/dendrite/app/config.json" <<EOF
{
  "backendUrl": "http://$TAILSCALE_IP:8080",
  "token": "NOAUTH"
}
EOF
echo "Updated dendrite/app/config.json"

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

# Cleanup on exit
trap "kill $BACKEND_PID 2>/dev/null" EXIT
