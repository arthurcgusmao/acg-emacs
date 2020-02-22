# My Emacs configurations

My Emacs configurations. Master branch supports Emacs 27; for Emacs 26 see
tags. It has been tested mainly on Ubuntu, but I do have a few (outdated)
adaptations to run it on Windows as well.

If you have contributions, questions, etc., feel free to get in touch.

Arthur


## Setup

### 1) Clone the repository

```console
git clone git@github.com:arthurcgusmao/acg-emacs.git ~/.config/emacs/
```

### 2) Change hardcoded paths

There are a few hardcoded filepaths in `acg-core.el` (top section), please
change them to what works for you.

### 3) Make OS-specific configurations

See additional configurations for the Operating system you are using:

- [Linux (Ubuntu)](./others/linux/)
- [Windows](./others/windows/)
- [WSL (Windows Subsystem for Linux)](./others/wsl/)
