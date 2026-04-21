#!/usr/bin/env bash
set -e
cd "$(dirname "$0")"

PROFILE="${IRIS_PROFILE:-main}"
SOPS_FILE="secrets/${PROFILE}.sops.yaml"
SOPS=$(which sops 2>/dev/null || ls "$HOME/.local/bin/sops" 2>/dev/null || find /nix/store -maxdepth 4 -name "sops" -type f 2>/dev/null | head -1)

if [ -z "$SOPS" ]; then
  echo "Error: sops not found. Install it or add to PATH."
  exit 1
fi

if [ ! -f "$SOPS_FILE" ]; then
  echo "Error: $SOPS_FILE not found"
  exit 1
fi

echo "Decrypting secrets from $SOPS_FILE..."
SOPS_AGE_KEY_FILE="$HOME/.config/sops/age/keys.txt" \
  "$SOPS" --decrypt "$SOPS_FILE" \
  | sed -n 's/^\([A-Z][A-Z_0-9]*\): \(.*\)/\1=\2/p' > .env.dev

set -a
source .env.dev
set +a

PID_FILE=".iris-layout.pid"

if [ -f "$PID_FILE" ]; then
  OLD_PID=$(cat "$PID_FILE")
  if kill -0 "$OLD_PID" 2>/dev/null; then
    echo "Stopping old iris-layout process (PID $OLD_PID)..."
    kill "$OLD_PID"
    for i in $(seq 1 20); do
      sleep 0.5
      if ! lsof -i :"${DEV_HTTP_PORT:-5173}" -sTCP:LISTEN -t >/dev/null 2>&1; then
        break
      fi
    done
  fi
  rm -f "$PID_FILE"
fi

echo "Starting iris-layout shadow-cljs watch..."
./node_modules/.bin/shadow-cljs watch app &
echo $! > "$PID_FILE"
wait $(cat "$PID_FILE")
rm -f "$PID_FILE"
