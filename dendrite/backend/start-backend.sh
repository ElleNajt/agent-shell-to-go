#!/usr/bin/env bash
# Start the dendrite backend server
# Usage: ./start-backend.sh [--build]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Get Tailscale IP
TAILSCALE_IP=$(tailscale ip -4 2>/dev/null || echo "")
if [ -z "$TAILSCALE_IP" ]; then
    echo "Error: Could not get Tailscale IP. Is Tailscale running?"
    exit 1
fi

# Token file location
TOKEN_FILE="$HOME/.agent-shell-api-token"
if [ ! -f "$TOKEN_FILE" ]; then
    echo "Creating token file at $TOKEN_FILE"
    openssl rand -hex 32 > "$TOKEN_FILE"
    chmod 600 "$TOKEN_FILE"
fi

# Build if requested or binary doesn't exist
if [ "$1" = "--build" ] || [ ! -f "./dendrite-backend" ]; then
    echo "Building backend..."
    go build -o dendrite-backend .
fi

# Configure Emacs
echo "Configuring Emacs..."
emacsclient --eval "(progn
  (setq agent-shell-to-go-mobile-backend-url \"http://$TAILSCALE_IP:8080\")
  (setq agent-shell-to-go-mobile-token-file \"$TOKEN_FILE\")
  (agent-shell-to-go-mobile-setup))" >/dev/null 2>&1 && echo "Emacs configured" || echo "Warning: Could not configure Emacs (is it running?)"

echo "Starting backend on $TAILSCALE_IP:8080"
exec ./dendrite-backend --listen "$TAILSCALE_IP:8080" --token-file "$TOKEN_FILE"
