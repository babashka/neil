# Changelog

[neil](https://github.com/babashka/neil): a CLI to add common aliases and features to `deps.edn`-based projects

See the [New Clojure project quickstart](https://blog.michielborkent.nl/new-clojure-project-quickstart.html) blog post for a gentle introduction into `neil`.

## ...

- [#258](https://github.com/babashka/neil/issues/258): `neil test` now exits with non-zero exit code when tests fail
- neil.el - a hook that runs after finding a package ([@agzam](https://github.com/agzam))
- neil.el - adds a function for injecting a found package into current CIDER session ([@agzam](https://github.com/agzam))

## 0.3.69

- [#245](https://github.com/babashka/neil/issues/245): neil.el - neil-executable-path now can be set to `clj -M:neil`
- [#251](https://github.com/babashka/neil/issues/251): Upgrade library deps-new to 0.10.3
- [#255](https://github.com/babashka/neil/issues/255): update maven search URL

## 0.3.68

- [#230](https://github.com/babashka/neil/issues/230): neil dep upgrade inserts git/url into upgraded dep ([@teodorlu](https://github.com/teodorlu))
- [#237](https://github.com/babashka/neil/issues/230): more specific error reporting on invalid github token ([@teodorlu](https://github.com/teodorlu))
- [#241](https://github.com/babashka/neil/issues/241): ignore missing deps file (instead of throwing) in `neil new` ([@bobisageek](https://github.com/bobisageek))

## 0.3.66

- [#215](https://github.com/babashka/neil/issues/215): `neil add kaocha` adds `:kaocha` alias with irregular indent ([@teodorlu](https://github.com/teodorlu))
- [#220](https://github.com/babashka/neil/issues/220): `neil add` print helptext rather than crash ([@teodorlu](https://github.com/teodorlu))
- [#227](https://github.com/babashka/neil/issues/227): Support `:neil/pinned true` to skip version updates when updating with `neil dep upgrade`

## 0.3.65

- [#209](https://github.com/babashka/neil/issues/209): add newlines between dependencies
- [#185](https://github.com/babashka/neil/issues/185): throw on non-existing library
- Bump `babashka.cli`
- Fetch latest stable `slipset/deps-deploy`, instead of hard-coding ([@vedang](https://github.com/vedang))
- Several emacs package improvements ([@agzam](https://github.com/agzam))

## 0.2.64
- [#205](https://github.com/babashka/neil/issues/205): `neil add build` now adds the latest version of `deps-deploy` from Clojars

## 0.2.63

- [#194](https://github.com/babashka/neil/issues/41): `dep search` in addition to Clojars, now also searches on Maven
- [#92](https://github.com/babashka/neil/issues/180): `neil dep upgrade`: delete rogue "`" character from helptext ([@teodorlu](https://github.com/teodorlu))

## 0.2.62

- Fix NPE during `neil dep upgrade`

## 0.2.61

- [#181](https://github.com/babashka/neil/issues/181): fix `neil --version`
- fix tests by referring to latest hiccup ([@teodorlu](https://github.com/teodorlu))
- [#180](https://github.com/babashka/neil/issues/180): `neil dep upgrade`: allow upgrading from an unstable version to the latest unstable version ([@teodorlu](https://github.com/teodorlu))
- [#180](https://github.com/babashka/neil/issues/180): `neil dep upgrade`: with `--unstable`, opt-into unstable library updates ([@teodorlu](https://github.com/teodorlu))
- [#183](https://github.com/babashka/neil/issues/183): Don't drop `:exclusions` when running `neil dep add` or `neil dep upgrade` ([@borkdude](https://github.com/borkdude) and [@teodorlu](https://github.com/teodorlu))

## 0.1.60 (2023-03-22)

- [#177](https://github.com/babashka/neil/issues/177): `neil dep add`: add latest unstable version if no stable versions are found ([@teodorlu](https://github.com/teodorlu))
- Continue upgrading deps even when some HTTP calls contain 5xx response codes

## 0.1.59 (2023-03-09)

- [#173](https://github.com/babashka/neil/issues/173): `neil new`: Support Git repos without tags

## 0.1.58 (2023-03-08)

- [#171](https://github.com/babashka/neil/issues/171): better stable version check
- [#170](https://github.com/babashka/neil/issues/170): always print and coerce `:version` as string

## 0.1.57 (2023-03-02)

- [#166]: Update `neil new -h` output to include list of provided templates ([@teodorlu](https://github.com/teodorlu))
- Upgrade git libs using explicit `:git/url` when available

## 0.1.56 (2023-02-17)

- [#163](https://github.com/babashka/neil/issues/163): Upgrade libraries, including deps-new to 0.5.0, which no longer uses [@seancorfield](https://github.com/seancorfield)'s tools.build wrapper in project templates

## 0.1.55 (2023-02-05)

- Bug fix for kaocha alias and script typos ([@KingMob](https://github.com/KingMob))

## 0.1.54 (2023-02-04)

- [#159](https://github.com/babashka/neil/issues/159): fix `pmap` + `requiring-resolve` issue

## 0.1.53 (2023-01-31)

- Clojars search improvements ([@tobias](https://github.com/tobias))

## 0.1.52 (2023-01-30)

- Fix fetching versions from maven central

## 0.1.51 (2023-01-30)

- Fix `neil dep add`
- Provide `bbin` install instructions installing a development version ([@teodorlu](https://github.com/teodorlu))

## 0.1.49 (2023-01-29)

- Fix incorrect "Requires clean working directory" error in `neil version`
- [#148](https://github.com/babashka/neil/issues/148): Always add latest kaocha version
- [#148](https://github.com/babashka/neil/issues/148): Print instructions for making a `bin/kaocha` script
- [#146](https://github.com/babashka/neil/issues/146): Prefer stable versions when running `neil dep upgrade`. Versions with any qualifier (like `-rc1`, `-alpha`, etc). are considered unstable.
- Fix `neil dep search` helptext formatting ([@teodorlu](https://github.com/teodorlu))
- Move `neil dep upgrade` helptext into subcommand ([@teodorlu](https://github.com/teodorlu))
- [#150](https://github.com/babashka/neil/issues/150): swap `babashka.curl` for `babashka/http-client`

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
