

# Configuration on Windows

This is a temporary workaround to be able to launch Emacs using `Windows key + emacs + RET`:

1. Place `acg-emacs.bat` under your Emacs binary path (e.g., `C:\Program Files (x86)\Emacs\emacs-26.1-x86_64\bin\acg-emacs.bat`)
2. Create a shortcut of the file in your desktop and rename it to "Emacs".

It is possible to create a bash script to be able to launch Emacs on a desired location from WSL (Windows Subsystem for Linux) by converting between paths in WSL and paths in Windows using `wslpath`. This is something I intend to do in the future.
