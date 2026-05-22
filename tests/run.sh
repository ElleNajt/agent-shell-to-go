#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEPS_DIR="$SCRIPT_DIR/deps"

if [ -n "$1" ]; then
  case "$1" in
    /*) FILES="$1" ;;
    *)  FILES="$SCRIPT_DIR/$1" ;;
  esac
else
  FILES="$(echo "$SCRIPT_DIR"/*-test.el)"
fi

emacs -batch \
  -L "$DEPS_DIR/shell-maker" \
  -L "$DEPS_DIR/acp" \
  -L "$DEPS_DIR/agent-shell" \
  -L "$DEPS_DIR/websocket" \
  -L "$PROJECT_ROOT" \
  -L "$SCRIPT_DIR" \
  $(printf -- '-l %s ' $FILES) \
  -f ert-run-tests-batch-and-exit
