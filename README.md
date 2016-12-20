# My EMACS configuration.

Hey! This is my emacs configuration folder.

All the configs I use are here. You can totally clone this repo if you want to.
However, there are still some paths hardcoded (since I haven't taken to the time
to seek how to properly handle those yet), so it won't be plug and play. Also
may require some knowledge on how emacs works.

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
