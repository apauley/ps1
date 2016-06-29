# A nicer bash prompt

A command-line executable that gives you a nice bash prompt

## Building

```bash
stack build
```

## Usage

Put the `ps1` binary somewhere in your path.

This command will copy it to your `~/bin` directory:
```bash
find .stack-work/install -type f -name 'ps1' | xargs -J % cp % ~/bin/
```

Put this in your `~/.bashrc`:

```bash
export PS1='$(ps1)'
```
