# QK Mode

As we all know, taking breaks is essential for maintaining productivity and overall wellness during long workdays. QK mode is a application for GNU Emacs that helps remind you to take a break during your workday.

![Screenshot](screenshot.png)

## Installation

To install QK mode, simply download the `qk-mode.el` file and save it to your Emacs Lisp directory (ex. `~/.emacs.d/elisp/`). Then, add the following line to your Emacs configuration file (ex. `~/.emacs.d/init.el`):

```
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'qk-mode)
```

## Usage

### Launch

To activate QK mode, simply run the following command.

`M-x qk`

This will switch the buffer and display a coffee cup in the center of the frame, with steam rising from the cup.

### Stop (Back to work)

The steam will continue to rise every second until specified count. After the break, disable QK mode by executing the `C-g` command.
