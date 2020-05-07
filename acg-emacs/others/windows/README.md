

# My configuration for Windows

## Installation

Although there is WSL, what worked best for me (in Aug/2019) was to install the Windows version of Emacs:

1. [Download](https://www.gnu.org/software/emacs/download.html#windows) Emacs for Windows;
2. Add the Windows binaries to your executable path (Cmd.exe path);


## Launch Emacs with ease

1. Use the default binary `emacsclient` to open desired files from both Windows Explorer (the file manager) and from the command prompt (Cmd.exe).
2. Link the execution of `acg-emacs.bat` to a keyboard shorcut using AutHotKey, cf. [here](https://github.com/arthurcgusmao/acg-windows/blob/master/startup/hotkeys.ahk).
3. Copy `acg-emacs-startup.bat` to Windows startup folder (usually `C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp`).


It is possible to create a bash script to be able to launch Emacs on a desired location from WSL (Windows Subsystem for Linux) by converting between paths in WSL and paths in Windows using `wslpath`. This is something I intend to do in the future.


If you want to be able to launch Emacs from the Windows startup panel (`Windows key + emacs + RET`), you can also:

1. Place `acg-emacs.bat` under any directory that is on your Windows binary path;
2. Create a shortcut of `acg-emacs.bat` in your desktop and rename it to "Emacs".


## Start daemon on startup

Follow [this tutorial](https://wikemacs.org/wiki/Emacs_server#MS_Windows).
