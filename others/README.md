# Some configurations for Emacs and the OS (linux)

## To install the desktop shortcut:

Run the command below (and don't forget to change the hardcoded filepath in `Exec` before doing so):

```bash
sudo desktop-file-install acg-emacs.desktop
```

## To make emacs the default application for text files:

Replace `gedit.desktop` by `acg-emacs.desktop` in `/usr/share/applications/defaults.list` file:

```bash
sudo sed -i 's/gedit\.desktop/acg-emacs\.desktop/g' /usr/share/applications/defaults.list
```


## To start emacs server on startup

Follow [this tutorial](http://wikemacs.org/wiki/Emacs_server).
