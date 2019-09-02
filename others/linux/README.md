# My configurations for Linux (Ubuntu)


## Emacs installation

On Ubuntu:
```console
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs26
```

## Add `acg-emacs.sh` to your path

`acg-emacs.sh` is a bash script to:

1. Create a daemon process in case none is running;
2. Connect to the running daemon;
3. Create frame in case none is open;
4. Open file or launch Emacs on existing frame.

The idea is that we are always going to have Emacs ready and open all files on the same frame.

Add `acg-emacs.sh` to your executable path, e.g., by creating a symlink in a directory that is in your path:

```console
ln -s $(realpath ./acg-emacs.sh) ~/.local/bin/acg-emacs
```

## Install the desktop shortcut

Run the command below (don't forget to change the hardcoded filepath in `Exec` if needed):

```console
sudo desktop-file-install acg-emacs.desktop
```

## Make emacs the default application for text files

Replace `gedit.desktop` by `acg-emacs.desktop` in `/usr/share/applications/defaults.list` by running:

```console
sudo sed -i 's/gedit\.desktop/acg-emacs\.desktop/g' /usr/share/applications/defaults.list
```


## Start emacs server on startup

According to [this tutorial](http://wikemacs.org/wiki/Emacs_server), the best way is to configure it through the init daemon `systemd`. Create a file `~/.config/systemd/user/emacsd.service` with the contents below:

```
[Unit]
Description=Emacs: the extensible, self-documenting text editor
Documentation=man:emacs(1) info:Emacs


[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval "(progn (setq kill-emacs-hook nil) (kill-emacs))"
Restart=on-failure
Environment=DISPLAY=:%i
TimeoutStartSec=0

[Install]
WantedBy=default.target
```

Then enable the new module with

```console
systemctl --user enable emacsd
```

You can check that the configurations have been applied by checking the output of

```console
systemctl --user list-units --type service --all
```
