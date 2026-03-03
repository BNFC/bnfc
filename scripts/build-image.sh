#!/usr/bin/env bash

set -euo pipefail

DEFAULT_TAG="bnfc/bnfc-test:9.12.2"
TAG="$DEFAULT_TAG"

show_help() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Builds the Docker image for BNFC development environment.

Options:
    -h, --help        display this help message and exit
    -t, --tag TAG     specify the image tag (default: $DEFAULT_TAG)
EOF
}

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            show_help
            exit 0
            ;;
        -t|--tag)
            if [[ -z "${2:-}" ]]; then
                echo "Error: --tag requires an argument." >&2
                exit 1
            fi
            TAG="$2"
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

docker build "$BASE_DIR" \
       -f "$BASE_DIR/Dockerfile.test" \
       --tag "$TAG"
