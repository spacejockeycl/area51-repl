> This is a fork of [fstamour/cl-repl](https://github.com/fstamour/cl-repl), which is a fork of [koji-kojiro/cl-repl](https://github.com/koji-kojiro/cl-repl)

This REPL is not designed to be full-featured. Its purpose it to provide just
enough functionality to allow a developer, new to Common Lisp, to work through
Practical Common Lisp without having to resort to using Emacs and Slime.

# Installation

Area51-REPL requires GNU Readline. You can install it via the following command:

```
$ brew install readline
```

Ensure that your terminal support 256 colors.

# Usage

```lisp
(ql:quickload :area51-repl)
(area51-repl:start)
```

To list available commands:

```lisp
CL-USER> .help
```

## .edit
Line editting in repl is sometimes painful. Area51-REPL allows you to edit code
with your favorite text editor.

```lisp
CL-USER> .edit <filename>
```

Area51-REPL invokes a text editor specified by `$EDITOR`.
After editting code, save and close it. Then repl will start to evaluate it.

# Contributing
Don't hesitate to open issues or to send PRs.
Any suggestions are always welcomed.

# Authors
[MJ Stahl](https://github.com/mjstahl)

[Francis St-Amour](https://github.com/fstamour)

[TANI Kojiro](https://github.com/koji-kojiro) (kojiro0531@gmail.com)

# License
Area51-REPL is distributed under [GPLv3](./LICENSE).
