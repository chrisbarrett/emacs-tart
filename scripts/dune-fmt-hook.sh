#!/usr/bin/env bash
set -euo pipefail

# Capture originally staged OCaml files before formatting
staged=$(git diff --cached --name-only --diff-filter=d -- "*.ml" "*.mli")

# Run the formatter (may modify files)
dune fmt || true

# Re-stage only the files that were originally staged
if [ -n "$staged" ]; then
    echo "$staged" | xargs git add
fi
