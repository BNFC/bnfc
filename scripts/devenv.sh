#!/usr/bin/env bash

set -euo pipefail

DEFAULT_IMAGE="bnfc/bnfc-test:9.12.2"
IMAGE="$DEFAULT_IMAGE"

show_help() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Starts a Docker-based development environment.

Options:
    -h, --help          display this help message and exit
    -i, --image IMAGE   specify the Docker image (default: $DEFAULT_IMAGE)
EOF
}

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -i|--image)
            if [[ -z "${2:-}" ]]; then
                echo "Error: --image requires an argument." >&2
                exit 1
            fi
            IMAGE="$2"
            shift 2
            ;;
        *)
            echo "Error: unknown argument $1" >&2
            show_help
            exit 1
            ;;
    esac
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
BASE_DIR="$(realpath "$SCRIPT_DIR/..")"

CUSERNAME=$(id -un)
CONTAINER_ID=$(( RANDOM % 256 ))

docker run -it --rm \
       --name "bnfc-dev-container-$CUSERNAME-$CONTAINER_ID" \
       --network host \
       -v "$BASE_DIR/source:/bnfc/source" \
       -v "$BASE_DIR/testing:/bnfc/testing" \
       -v "$BASE_DIR/examples:/bnfc/examples" \
       -e LC_ALL=C.UTF-8 \
       -w /bnfc \
       "$IMAGE" \
       bash
