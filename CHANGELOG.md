# Changelog

[neil](https://github.com/babashka/neil): a CLI to add common aliases and features to `deps.edn`-based projects

See the [New Clojure project quickstart](https://blog.michielborkent.nl/new-clojure-project-quickstart.html) blog post for a gentle introduction into `neil`.

## Unreleased

- Fix incorrect "Requires clean working directory" error in `neil version`
- Always add latest kaocha version
- Print instructions for making a `bin/kaocha` script
- Fix `neil dep search` helptext formatting ([@teodorlu](https://github.com/teodorlu))

## 0.1.48 (2023-01-20)

- Print a message and exit when the github rate-limit is reached ([#136](https://github.com/babashka/neil/issues/136)) ([@russmatney](https://github.com/russmatney))
- Friendlier env vars for neil github token usage ([#136](https://github.com/babashka/neil/issues/136)) ([@russmatney](https://github.com/russmatney))
  - `BABASHKA_NEIL_DEV_GITHUB_USER` -> `NEIL_GITHUB_USER`
  - `BABASHKA_NEIL_DEV_GITHUB_TOKEN` -> `NEIL_GITHUB_TOKEN`
  The old env vars are left in-place as a fallback, but may be removed in a future version.
- `neil dep add`: Always write library names as namespaced symbols ([@teodorlu](https://github.com/teodorlu))
  - Old behavior: `neil dep add http-kit` writes `http-kit {:mvn/version "2.7.0-alpha1"}` to `deps.edn`
  - New behavior: `neil dep add http-kit` writes `http-kit/http-kit {:mvn/version "2.7.0-alpha1"}` to `deps.edn`

## 0.1.47 (2022-10-19)

- `neil dep add` now supports `--tag` and `--latest-tag` ([@russmatney](https://github.com/russmatney))
- `neil dep upgrade` enhancements ([@russmatney](https://github.com/russmatney))
  - when `:git/tag` is used, upgrades to the repo's latest tag (instead of the latest sha)
  - support prefixless `:tag` and `:sha` coords, which tools.deps supports (for backwards compatibility)
  - upgrade alias deps (now the default).
    - `neil dep upgrade --alias tests` supports upgrading deps for a particular alias.
    - `neil dep upgrade --no-aliases` supports upgrading _only_ the project deps.
  - the `neil dep upgrade --dry-run` output can now be piped back into `neil dep
    add`, so you can now select a single upgrade from a list of available via `fzf`:
    ```
    neil dep upgrade --dry-run | fzf | xargs ./neil dep add
    ```
- Don't add libraries when version is unknown ([@russmatney](https://github.com/russmatney))

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
