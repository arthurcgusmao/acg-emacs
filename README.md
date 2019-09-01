# My Emacs configurations

This repository contains my Emacs configurations. It has been tested on Ubuntu and Windows.
There are a few hardcoded filepaths in `acg-core.el` (top section), please change them to what
works for you.

Feel free to get in touch if you want to,

Arthur

## Setup

### 1) Clone the repository

Clone this repo into your `~/.emacs.d` folder (default folder name will be `acg-emacs`).

### 2) Add path to list and require core

Add to the (or create a) `~/.emacs.d/init.el` file the following:

```lisp
;; loads acg-emacs
(add-to-list 'load-path "~/.emacs.d/acg-emacs")
(require 'acg-core)
```

### 3) Make OS configurations

See additional configurations for the OS type:

- [Linux (Ubuntu)](./others/linux/)
- [Windows](./others/windows/)
