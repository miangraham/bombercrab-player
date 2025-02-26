#!/run/current-system/sw/bin/bash

set -o errexit
set -o pipefail

# Default values. Copy .env.example to .env and edit the values there to override.
UPLOAD_SERVER_ADDRESS=${UPLOAD_SERVER_ADDRESS:-127.0.0.1:8765}
API_KEY=${API_KEY:-abcdef}
CRATE_NAME=${CRATE_NAME:-player}

if [ -r .env ]; then
    source .env
    echo "Loaded env variables from .env."
else
    echo "Copy .env.example to .env to conveniently set env variables."
fi

cargo build --release -p ${CRATE_NAME} --target wasm32-unknown-unknown

FILE="target/wasm32-unknown-unknown/release/${CRATE_NAME}.wasm"
echo "Submitting ${FILE} to ${UPLOAD_SERVER_ADDRESS}."
curl -X POST -H "Api-Key: ${API_KEY}" --data-binary @${FILE} http://${UPLOAD_SERVER_ADDRESS}/
