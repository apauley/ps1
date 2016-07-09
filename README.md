# A nicer bash prompt

A command-line executable that gives you a nice bash prompt

## Building and Install

This will compile and copy `ps1` to `~/.local/bin`:
```bash
stack install
```

## Usage

Ensure that `~/.local/bin` is in your path.

Put this in your `~/.bashrc`:

```bash
export PS1='$(ps1)'
```
