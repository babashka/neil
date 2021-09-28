# neil

A handy script to add common aliases and features to `deps.edn`-based project.

## Installation

This script requires
[babashka](https://github.com/babashka/babashka#installation) to be
installed. Then you can download the `neil` script to somewhere on your `PATH`
and invoke it from anywhere on your system.

To automate this process, you can install `neil` using homebrew:

```
$ brew install babashka/brew/neil
```

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

## License

TBD
