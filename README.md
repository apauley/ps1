# A nicer bash prompt

A command-line executable that gives you a nice bash prompt

## Building and Install

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
PS1='$(ps1 -t origin/master)\n\[\h\]:\[\w\]$ '
```
