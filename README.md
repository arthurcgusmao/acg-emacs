# My Emacs configurations

My Emacs configurations. Master branch supports Emacs 28; for older versions
see tags. There are tags for the lattest commit run on a particular operating
system too.

If you have contributions, questions, etc., feel free to get in touch.

Arthur


## Setup

### 1) Clone the repository

```console
git clone git@github.com:arthurcgusmao/acg-emacs.git ~/.config/emacs/
```

### 2) Change hardcoded paths

There are a few hardcoded filepaths in `acg-emacs/acg-core.el` (top section),
please change them to what works for you.

### 3) Make OS-specific configurations

See additional configurations for the Operating system you are using:

- [Linux (Ubuntu)](./acg-emacs/others/linux/)
- [Windows](./acg-emacs/others/windows/)
- [WSL (Windows Subsystem for Linux)](./acg-emacs/others/wsl/)
- [MacOS](./acg-emacs/others/macos/)
