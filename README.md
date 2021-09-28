# neil

A CLI to add common aliases and features to `deps.edn`-based projects.

## Installation

### Homebrew (Linux and macOS)

```
$ brew install babashka/brew/neil
```

### Manual

- Install [babashka](https://github.com/babashka/babashka#installation)
- Download the `neil` script to somewhere on your `PATH`.

### Scoop (Windows)

A scoop package for Windows is coming soon.

## Usage

Type `neil` to see the help.

Example:

``` clojure
$ neil add test
```

This will add the Cognitect
[test-runner](https://github.com/cognitect-labs/test-runner) to your `deps.edn`
so you can execute:

``` clojure
$ clojure -X:test
```

A similar option is support for [kaocha](https://github.com/lambdaisland/kaocha):

```
$ neil add kaocha
```

To change the alias you can provide an option like:

```
$ neil add kaocha :alias kaocha2
```

## Roapmap

- Add `build` feature which adds `:build` alias and vanilla `build.clj`
- Better formatting of `deps.edn`, possibly using a formatter like `cljfmt`
- Add `bb.edn`-related features for invoking `test` and `build` tasks

## License

TBD
