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

Type `neil` to see the help:

```
Usage: neil <subcommand>

Subcommands:

add

  test: adds cognitect test runner to :test alias.
  build: adds tools.build build.clj file and :build alias.
  kaocha: adds kaocha test runner to :koacha alias.

Override alias names with :alias option:

neil add test :alias test2
```

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

- Better formatting of `deps.edn`, possibly using a formatter like `cljfmt`
- Add `bb.edn`-related features for invoking `test` and `build` tasks
- Consider `neil test :only foo.bar` which invokes `clojure -M:test -n foo.bar`

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

TBD
