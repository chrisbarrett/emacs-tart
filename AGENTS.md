# tart

Elisp type checker in OCaml.

## Commands

build: `Bash(command="nix develop --command dune build 2>&1")`

## Required Skills

| Condition              | Skill                                 |
| ---------------------- | ------------------------------------- |
| Git commits            | `Skill(skill="git:committing")`       |
| Emacs interaction      | `Skill(skill="emacs:emacsclient")`    |
| Specs, updating DESIGN | `Skill(skill="claude:writing-specs")` |

## Constraints

> [!CRITICAL]
>
> - Write `.mli` for every `lib/**/*.ml`
> - Update `DESIGN.md` when completing specs
> - Keep `DEVELOPMENT.md` current

## Reference

| Resource        | Location          |
| --------------- | ----------------- |
| Design          | `DESIGN.md`       |
| Implementation  | `docs/reference/` |
| Research        | `docs/research/`  |
| Active specs    | `specs/`          |
| Archived specs  | `specs/.archive/` |
| Progress        | `progress.txt`    |
