

# Running Emacs on WSL (with graphical interface)

Install it as you normally would in Linux, and launch it from the command line having Xlaunch installed to run the graphical interface. The only section of my configurations for Linux that I had to change is running Emacs at startup, which I had to setup with an external program (such as https://github.com/troytse/wsl-autostart) since the init daemon `systemd` is currently not supported on WSL.

## Further Adaptations/Integrations

Some adaptations are needed to further improve the WSL Emacs experience. Add the following to your `init.el`:

```lisp
;; wsl adaptations
(acg/load-all-in-directory (concat acg-emacs-dir "others/wsl"))
```

## Launching WSL Emacs from Windows

Copy the batch file `./wsl-acg-emacs.bat` to a directory that is in your Windows path and then use it to open whatever file you'd like on Windows explorer. Currently it is only working when the Emacs frame is already open (via wsl command line); @todo: improve.
