# neil

A CLI to add common aliases and features to `deps.edn`-based projects.

## Installation

### Homebrew (Linux and macOS)

```
$ brew install babashka/brew/neil
```

### Scoop (Windows)

```
$ scoop bucket add scoop-clojure https://github.com/littleli/scoop-clojure
$ scoop install neil
```
For detailed information about scoop installer check [scoop-clojure](https://github.com/littleli/scoop-clojure).

### Nix

```bash 
$ nix-shell -p neil

# Alternatively, if your nix channel doesn't have neil yet:
$ nix-shell -I nixpkgs=channel:nixos-unstable -p neil
```

### Manual

- Install [babashka](https://github.com/babashka/babashka#installation)
- Download the `neil` script to somewhere on your `PATH`. In Windows, also
  download the `neil.bat` script and place it in the `PATH`.

## Status

As we're still finding out the best UX, `neil` may undergo breaking changes from
version to version.

## Usage

Type `neil` to see the help:

```
Usage: neil <subcommand> <options>

Most subcommands support the options:

- :alias - override alias name
- :deps-file - override deps.edn file name

Subcommands:

add

  - dep: adds :lib, a fully qualified symbol, to deps.edn :deps. Example:

    Options:

    :lib - Fully qualified symbol. :lib keyword may be elided when lib name is provided as first option.
    :version - Optional version. When not provided, picks newest version from Clojars or Maven Central.
    :sha - When provided, assumes lib refers to Github repo.
    :latest-sha - When provided, assumes lib refers to Github repo and then picks latest SHA from it.
    :deps/root - Set :deps/root to given value
    :as - Use as dependency name in deps.edn

  - test: adds cognitect test runner to :test alias.

  - build: adds tools.build build.clj file and :build alias.

    Options:

    :deps-deploy true - adds deps-deploy as dependency and deploy task in build.clj

  - kaocha: adds kaocha test runner to :koacha alias.
```

### add dep

This will add the newest version of clj-kondo to the `:deps` map in `deps.edn`:

```
$ neil add dep :lib clj-kondo/clj-kondo
```

The `:lib` keyword may be elided if the libname is the first argument after `dep`:

```
$ neil add dep clj-kondo/clj-kondo
```

The `add dep` command will always overwrite an existing dependency.

To add a git library from Github you can use `:sha` to provide a SHA or
`:latest-sha` to pick the latest sha from the default branch:

```
$ neil add dep borkdude/sci :latest-sha true
```

### add build

To add [tools.build](https://clojure.org/guides/tools_build) related features, use:

```
$ neil add build :deps-deploy true
```

After that you can run tasks like:

```
$ clojure -T:build uber
$ clojure -T:build deploy
```

### add test

``` clojure
$ neil add test
```

This will add the Cognitect
[test-runner](https://github.com/cognitect-labs/test-runner) to your `deps.edn`
so you can execute:

``` clojure
$ clojure -X:test
```

A similar option is supported for
[kaocha](https://github.com/lambdaisland/kaocha):

```
$ neil add kaocha
```

To change the alias you can provide an option like:

```
$ neil add kaocha :alias kaocha2
```

## Roapmap

- Add `bb.edn`-related features for invoking `test` and `build` tasks
- Consider `neil test :only foo.bar` which invokes `clojure -M:test -n foo.bar`
- Option to add `cljs-test-runner`

## Contributing

If this project shows potential to you, I'd be happy to discuss and receive
contributions.

## Dev

See
[neil.rb](https://github.com/babashka/homebrew-brew/blob/main/Formula/neil.rb)
for the brew Formula. You can install this formula locally with:

```
$ brew reinstall --build-from-source ./neil.rb`
```

## License

Copyright © 2021 Michiel Borkent

Distributed under the MIT License. See LICENSE.
