# A git-aware bash prompt

A command-line executable that generates a git-aware bash prompt.

The idea is to extract most of the information you frequently need
to get using `git status`,  `git log` etc. and present it in your prompt.

## Build and Install

This will compile and copy `ps1` to `~/.local/bin`:
```bash
stack install
```

## Usage

Ensure that `~/.local/bin` is in your path.

Have a look at the options:

```bash
ps1 --help
```

Put something like this in your `~/.bashrc`:

```bash
PS1='$(ps1 ml)'
```

Change `ml` to `sl` for a single-line prompt

You can add a file called `ps1.cfg` to any of your git repositories.
This allows you to specify another upstream branch to track if needed.

Example contents:

```
# https://github.com/apauley/ps1#readme
# This tells the prompt to display `Diverged from origin/develop` if needed
track-branch = origin/develop
```

## Build the Executable using Stack within Docker

```bash
$ docker run -v ~/.stack:/root/.stack -v ~/.local/bin:/root/.local/bin -v ${PWD}:/ps1 -it --rm haskell:8.0.1 /bin/bash
$ cd /ps1
$ stack config set system-ghc --global true
$ stack install --allow-different-user
```
