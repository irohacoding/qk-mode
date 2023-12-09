# QK Mode

As we all know, taking breaks is essential for maintaining productivity and overall wellness during long workdays. QK mode is an application for GNU Emacs that helps remind you to take a break during your workday.

![Screenshot](screenshot.png)

## Installation

### Manual
To install QK mode, simply download the `qk-mode.el` file and save it to your Emacs Lisp directory (e.g., `~/.emacs.d/elisp/`). Then, add the following line to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):

```
(add-to-list 'load-path "~/.emacs.d/elisp")
(require 'qk-mode)
```

## Usage

### Launch

To activate QK mode, simply run the following command.

`M-x qk`

This will switch the buffer and display a coffee cup in the center of the frame, with steam rising from the cup.

### Finish

The steam will continue to rise every 10 seconds until a specified count. After the break, finish QK mode by executing the `C-g` command.

## Customization

### Steaming count

When launching QK mode, steam rises from the coffee cup a specified number of times and then disappears.
You can specify the number of times the steam rises.

`M-x customize-option` `RET` `qk-mode-steaming-count` `RET`

Default: 8 times

(Changed variable name on version 0.2.4 (eliminate max-). So if you already set count, need to re-set count. xP)

### Show favorite words after steaming

After the specified number of steaming times, you can display your favorite words above the steam.
You can choose whether to display these words or not.

`M-x customize-option` `RET` `qk-mode-show-words` `RET`

Default: t (on)

### Change favorite words

Feel free to set your favorite words and see them after breaks to get back to work energetically.
Currently, only a single line is supported.

`M-x customize-option` `RET` `qk-mode-favorite-words` `RET`

Default: less is more

---

Taking breaks at regular intervals is important for mental and physical health. Research has shown that taking breaks can improve focus, creativity, and cognitive function. In addition, taking breaks can prevent burnout and reduce stress levels.

QK mode is a helpful tool for those who may forget to take breaks. We hope that QK mode will help you take the breaks you need to stay focused and productive!

Enjoy!!
