# Tuimake

[![Build](https://github.com/fwcd/tuimake/actions/workflows/build.yml/badge.svg)](https://github.com/fwcd/tuimake/actions/workflows/build.yml)

A wrapper around (GNU) Make that parses its debug output and visualizes the execution of rules.

## Usage

Tuimake can work as a drop-in replacement in commands using Make (note however that it is intended for interactive use, `tuimake` does not terminate once `make` has finished executing). For example, if you use

```
make all
```

to build your project, just run

```
tuimake all
```

from the same folder. You can navigate the output and resize the horizontal split using your arrow-keys or Vim-style using `hjkl`.

> Note that you cannot use `make`'s debug flags since `tuimake` relies on them internally.
