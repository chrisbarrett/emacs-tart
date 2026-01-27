#!/usr/bin/env bash
# Run Emacs ERT tests for tart-mode.el
#
# Usage: ./scripts/run-emacs-tests.sh
#
# Discovers all *-tests.el and *-e2e-tests.el files in lisp/ and runs them
# with `emacs --batch`. Requires the tart executable to be available.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
LISP_DIR="$PROJECT_ROOT/lisp"

# Ensure tart is built and set up PATH
TART_BIN="$PROJECT_ROOT/_build/default/bin"
if [[ -x "$TART_BIN/main.exe" ]]; then
    # Create a symlink so tart is available as "tart"
    if [[ ! -e "$TART_BIN/tart" ]]; then
        ln -sf main.exe "$TART_BIN/tart"
    fi
    export PATH="$TART_BIN:$PATH"
elif ! command -v tart &>/dev/null; then
    echo "Error: tart executable not found. Run 'dune build' first." >&2
    exit 1
fi

# Find all test files
test_files=()
while IFS= read -r -d '' file; do
    test_files+=("$file")
done < <(find "$LISP_DIR" \( -name '*-tests.el' -o -name '*-e2e-tests.el' \) -print0 2>/dev/null)

if [[ ${#test_files[@]} -eq 0 ]]; then
    echo "No test files found in $LISP_DIR"
    exit 0
fi

echo "Running Emacs tests..."
echo "Test files: ${test_files[*]}"
echo

# Build load-path arguments
load_path_args=(-L "$LISP_DIR")

# Build load arguments for test files
load_args=()
for file in "${test_files[@]}"; do
    load_args+=(-l "$file")
done

# Run tests with emacs --batch
# Use EMACS env var if set, otherwise default to emacs
EMACS="${EMACS:-emacs}"

exec "$EMACS" --batch \
    "${load_path_args[@]}" \
    -l ert \
    "${load_args[@]}" \
    -f ert-run-tests-batch-and-exit
