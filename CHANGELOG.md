# Changelog

## Unreleased

- `neil dep add` now supports `--tag` and `--latest-tag`
- `neil dep upgrade`
  - when `:git/tag` is used, upgrades to the repo's latest tag (instead of the latest sha)
  - support `:tag` and `:sha`, which tools.deps supports (for backwards compatibility)
  - upgrade alias deps (now the default).
    - `neil dep upgrade --alias tests` supports upgrading deps for a particular alias.
    - `neil dep upgrade --no-aliases` supports upgrading _only_ the project deps.

## 0.1.46 (2022-10-12)

- Introduce `neil dep upgrade` API for upgrading existing dependencies. By [@teodorlu](https://github.com/teodorlu) and [@russmatney](https://github.com/russmatney).
- Add nix flake. By [@jlesquembre](https://github.com/jlesquembre).
- `neil version` improvements. By [@rads](https://github.com/rads).

## 0.1.45 (2022-08-30)

- Add `neil version` subcommands
  - `neil version` (updated, now includes project version)
  - `neil version set [version]`
  - `neil version [major|minor|patch] [version]`
  - `neil version tag`

## 0.1.43

- Save project name in `:aliases {:neil {:project {:name ...}}}`
- Generate default test with `neil add test` based on project name
- Use project name in `build.clj`

## 0.1.41

- Add `neil test`

## 0.1.40

- Support `neil dep search --help`
- Upgrade deps

## 0.1.39

- Minor improvements for `new`

## 0.1.36

- Add `neil new`: invokes `deps-new` with template. Recommended template to use with `neil` is `scratch`. Type `neil new --help` to see help.

- Add `neil add nrepl`: add `:nrepl` alias to `deps.edn`
