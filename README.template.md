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

### Clojure

Add the following alias to your global or project-local `deps.edn`:

``` clojure
:neil {:deps {io.github.babashka/neil {:git/tag "v0.1.41"
                                       :git/sha "77288d4"}}
       :main-opts ["-m" "babashka.neil"]}
```

Then invoke `clj -M:neil`.

### Manual

- Install [babashka](https://github.com/babashka/babashka#installation)
- Download the [`neil`](https://raw.githubusercontent.com/babashka/neil/main/neil) script to somewhere on your `PATH`. In Windows, also
  download the [`neil.bat`](https://raw.githubusercontent.com/babashka/neil/main/neil.bat) script and place it in the `PATH`.

## Status

As we're still finding out the best UX, `neil` may undergo breaking changes from
version to version.

## Usage

Type `neil` to see the help:

```
{{help}}
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

### dep search

Search Clojars for a string in any attribute of an artifact:

```
$ neil dep search "babashka.nrepl"
:lib babashka/babashka.nrepl :version 0.0.6
```

Note that Clojars stores the namespace and name of a library as separate attributes, so searching for a ns-qualified library will not necessarily return any matches:

```
$ neil dep search "babashka/babashka.nrepl"
Unable to find babashka/babashka.nrepl on Clojars.
```

But a search string can be matched in a library's description:

```
$ neil dep search "test framework"
```
will return libraries with 'test framework' in their description.

### license list

List/search for licenses that can be added to a project with `neil`. This functionality uses Github's license API, 
which is also used by [choosealicense.com](https://choosealicense.com/). With no search term, a list of 
commonly-used licenses is returned:

```
$ neil license list
:license agpl-3.0 :name GNU Affero General Public License v3.0
:license apache-2.0 :name Apache License 2.0
...
```

A search term can be added to filter the commonly-used list with a case-insensitive search against the license name:

```
$ neil license list "lesser general"
:license lgpl-2.1 :name GNU Lesser General Public License v2.1
```

The full collection of available licenses can be found in the [license API repo](https://github.com/github/choosealicense.com/tree/gh-pages/_licenses).

`license search` is an alias for `license list`.

### license add

Retrieve license text from Github's license API and write it to a file. See the `license list` help for details on available licenses.

```
$ neil license add :license mit :file myproj/license.txt
```

Will write the MIT license to the file myproject/license.txt. The `:license` keyword can be left out if the license key is the first argument,
and `:file` defaults to LICENSE, so a minimal usage:

```
$ neil license add epl-1.0
```

Will create a LICENSE file in the current directory with the EPL 1.0 text.

## Emacs Integration

[neil.el](https://github.com/babashka/neil/blob/main/neil.el) is a companion Emacs package.

Load it using your preferred Emacs package manager, e.g., for Doom Emacs:

```emacs-lisp
;; packages.el

(package! neil :recipe (:host github :repo "babashka/neil" :files ("*.el")))

;; config.el

(use-package! neil
  :config 
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t))

```

## Github's Rate Limit

Github's API has a 60 hit/hour rate-limit. The workaround for this is creating a
[personal access
token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token)
and setting two env vars:

- `BABASHKA_NEIL_DEV_GITHUB_USER`
- `BABASHKA_NEIL_DEV_GITHUB_TOKEN`

## Roadmap

- Add `bb.edn`-related features for invoking `test` and `build` tasks
- Option to add `cljs-test-runner`

## Contributing

If this project shows potential to you, I'd be happy to discuss and receive
contributions.

## Dev

See
[neil.rb](https://github.com/babashka/homebrew-brew/blob/main/Formula/neil.rb)
for the brew Formula. You can install this formula locally with:

```
$ brew reinstall --build-from-source ./neil.rb
```

## License

Copyright © 2022 Michiel Borkent

Distributed under the MIT License. See LICENSE.
