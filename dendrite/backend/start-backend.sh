#!/usr/bin/env bash
# Start the dendrite backend server
# Usage: ./start-backend.sh [--build]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Build if requested or binary doesn't exist
if [ "$1" = "--build" ] || [ ! -f "./dendrite-backend" ]; then
    echo "Building backend..."
    go build -o dendrite-backend .
fi

# Configure Emacs with auto-detected IP
TAILSCALE_IP=$(tailscale ip -4 2>/dev/null || echo "")
if [ -n "$TAILSCALE_IP" ]; then
    echo "Configuring Emacs..."
    emacsclient --eval "(progn
      (setq agent-shell-to-go-mobile-backend-url \"http://$TAILSCALE_IP:8080\")
      (agent-shell-to-go-mobile-setup))" >/dev/null 2>&1 && echo "Emacs configured" || echo "Warning: Could not configure Emacs (is it running?)"
fi

echo "Starting backend..."
exec ./dendrite-backend
