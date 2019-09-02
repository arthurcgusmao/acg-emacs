

# My configuration for Windows

## Installation

Although there is WSL, what worked best for me (in Aug/2019) was to install the Windows version of Emacs:

1. [Download](https://www.gnu.org/software/emacs/download.html#windows) Emacs for Windows;
2. Add the Windows binaries to your executable path (Cmd.exe path);


## Launch Emacs with ease

@TODO

This is a temporary workaround to be able to launch Emacs using `Windows key + emacs + RET`:

1. Place `acg-emacs.bat` under your Emacs binary path (e.g., `C:\Program Files (x86)\Emacs\emacs-26.1-x86_64\bin\acg-emacs.bat`)
2. Create a shortcut of the file in your desktop and rename it to "Emacs".

It is possible to create a bash script to be able to launch Emacs on a desired location from WSL (Windows Subsystem for Linux) by converting between paths in WSL and paths in Windows using `wslpath`. This is something I intend to do in the future.


## Start daemon on startup

Follow [this tutorial](https://wikemacs.org/wiki/Emacs_server#MS_Windows).
