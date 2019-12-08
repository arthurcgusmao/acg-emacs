# My Emacs configurations

This repository contains my Emacs configurations. It has been tested on Ubuntu and Windows (on Windows, I have been using these configs mainly running Emacs on WSL).

If you have contributions, questions, etc., feel free to get in touch.

Arthur


## Setup

### 1) Clone the repository

Clone this repo into your `~/.emacs.d` folder (default folder name will be `acg-emacs`).

### 2) Change hardcoded paths

There are a few hardcoded filepaths in `acg-core.el` (top section), please change them to what
works for you.

### 3) Add path to list and require core

Add to the `~/.emacs.d/init.el` file the following content (create the file if necessary):

```lisp
;; loads acg-emacs
(add-to-list 'load-path "~/.emacs.d/acg-emacs")
(require 'acg-core)
```

### 4) Make OS-specific configurations

See additional configurations for the Operating system you are using:

- [Linux (Ubuntu)](./others/linux/)
- [Windows](./others/windows/)
- [WSL (Windows Subsystem for Linux)](./others/wsl/)
