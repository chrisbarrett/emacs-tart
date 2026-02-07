# Spec 41: Emacs Source Corpus

Clone Emacs source in XDG cache for testing against real Elisp.

**Deps:** [Spec 29][]

## Links

### Deps
[Spec 29]: ./29-emacs-coverage.md

## Constraints

| Constraint | Detail |
|------------|--------|
| XDG compliant | `$XDG_CACHE_HOME/tart/emacs-src/` |
| Lazy clone | Only clone when needed |
| Shallow | Minimize disk/network via `--depth 1` |
| Version control | Tags, commits, branches |
| Offline graceful | Clear error if network unavailable |

## Output

```
lib/corpus/
├── emacs_corpus.ml
└── emacs_corpus.mli
bin/main.ml  ; Add corpus subcommands
```

## Requirements

### R1: Shallow clone if not present

**Given** cache dir missing **When** corpus accessed **Then** shallow clone from
`https://git.savannah.gnu.org/git/emacs.git`

```bash
git clone --depth 1 --branch <tag> <url> <path>
```

Emacs history spans decades; shallow clone reduces ~3GB to ~200MB.

**Verify:** `rm -rf ~/.cache/tart/emacs-src && tart corpus checkout emacs-31.1 && du -sh ~/.cache/tart/emacs-src`

### R2: Reuse existing clone

**Given** cache exists **When** corpus accessed **Then** no network activity

**Verify:** Second `tart corpus list` completes in <1s

### R3: XDG_CACHE_HOME

**Given** `XDG_CACHE_HOME=/custom` **When** cloned **Then** `/custom/tart/emacs-src/`

**Verify:** `XDG_CACHE_HOME=/tmp/xdg tart corpus list` creates `/tmp/xdg/tart/emacs-src/`

### R4: Default ~/.cache

**Given** XDG unset **When** cloned **Then** `~/.cache/tart/emacs-src/`

**Verify:** `unset XDG_CACHE_HOME && tart corpus path` outputs `$HOME/.cache/tart/emacs-src`

### R5: Checkout tag (shallow fetch)

**Given** existing clone **When** `tart corpus checkout emacs-29.1` **Then**
fetch tag shallowly if not present, checkout

```bash
git fetch --depth 1 origin tag <tag> --no-tags
git checkout <tag>
```

**Verify:** `tart corpus checkout emacs-29.1 && git -C "$(tart corpus path)" describe --tags`

### R6: Checkout commit

**Given** existing clone **When** `tart corpus checkout <sha>` **Then** checkout that commit

**Verify:** `git -C "$(tart corpus path)" rev-parse HEAD` matches

### R7: Auto-detect system Emacs version

**Given** no explicit version **When** `tart corpus checkout` **Then** query system Emacs, checkout matching tag

**Verify:** Tag matches Emacs version

### R8: Version detection via emacs --version

**Given** system Emacs installed **When** auto-detecting **Then** parse `emacs --version`, map to `emacs-{major}.{minor}`

**Verify:** First line matches detected version

### R9: Explicit version for CI

**Given** `--emacs-version 29.1` **When** checkout **Then** use `emacs-29.1` regardless of system

**Verify:** Works without Emacs installed

### R10: List .el files

**Given** checked-out corpus **When** `tart corpus list` **Then** all `.el` files, absolute paths

**Verify:** `tart corpus list | wc -l` returns 3000+

### R11: Include generated files

**Given** corpus **When** listing **Then** include `loaddefs.el` etc.

**Verify:** `tart corpus list | grep -c loaddefs`

### R12: Network failure (fresh clone)

**Given** no cache + no network **When** corpus accessed **Then** exit 1, clear message

**Verify:** `rm -rf ~/.cache/tart/emacs-src && timeout 1 tart corpus list 2>&1`

### R13: Network failure (fetch)

**Given** cache exists but tag missing **When** fetch fails **Then** exit 1, suggest local tag

**Verify:** `tart corpus checkout emacs-99.0 2>&1`

### R14: Report path

**Given** corpus configured **When** `tart corpus path` **Then** output absolute path

**Verify:** Valid directory path

### R15: Clean cache

**Given** existing corpus **When** `tart corpus clean` **Then** remove dir, report bytes freed

**Verify:** `tart corpus clean && ! [ -d "$(tart corpus path)" ]`

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Network error |
| 2 | Invalid version/tag |
| 3 | Emacs not found (auto-detect) |

## Non-Requirements

- Multiple concurrent versions
- Windows support

## Tasks

- [x] [R3-4] XDG path resolution
- [x] [R1-2] Shallow clone-if-needed
- [x] [R5-6] Shallow tag fetch + checkout
- [x] [R7-8] Version auto-detection
- [x] [R9] `--emacs-version` option
- [x] [R10-11] File listing
- [x] [R12-13] Network error handling
- [x] [R14] `corpus path`
- [x] [R15] `corpus clean`
- [x] Wire CLI subcommands
- [x] Unit tests

## Status

Complete
