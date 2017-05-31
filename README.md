# My Emacs configurations

This repository contains all the configurations I use in Emacs.

You can totally clone it if you want to. However, it was only tested on linux, so be aware that
it may not work on Windows/MacOS. You may also find some hardcoded filepaths at the
`acg-core.el` file, please change them to what works for you.

Feel free to get in touch if you want to,

Arthur

## Setup

### 1) Clone the repository

Clone this repo into your `~/.emacs.d` folder (default folder name will be
`acg-emacs`).

### 2) Add path to list and require core

Add to the (or create a) `~/.emacs.d/init.el` file the following:

```lisp
;; loads acg-emacs
(add-to-list 'load-path "~/.emacs.d/acg-emacs")
(require 'acg-core)
```
