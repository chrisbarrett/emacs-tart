#!/usr/bin/env bash
# Run round-trip parse-print-parse tests against the Emacs corpus.
#
# Usage:
#   ./scripts/run-roundtrip.sh [--with-emacs] [--no-cache] [FILES...]
#
# With no arguments, tests all .el files from the Emacs corpus.
# Requires 'tart corpus checkout' to have been run first (unless FILES given).

set -euo pipefail

# Find tart binary: prefer local build, fall back to PATH
TART="${TART:-}"
if [[ -z "$TART" ]]; then
  script_dir="$(cd "$(dirname "$0")" && pwd)"
  repo_root="$(dirname "$script_dir")"
  local_tart="$repo_root/_build/default/bin/main.exe"
  if [[ -x "$local_tart" ]]; then
    TART="$local_tart"
  elif command -v tart >/dev/null 2>&1; then
    TART="tart"
  else
    echo "error: tart not found; build with 'dune build' or set TART" >&2
    exit 2
  fi
fi

exec "$TART" roundtrip "$@"
