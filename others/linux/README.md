# My configurations for Linux (Ubuntu)


## Emacs installation

### Using ppa repository

```console
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install emacs26
```

### Building from source

The last time I installed from the ppa, Emacs had problems with dynamic modules (even though it was build with the right option, I wasn't able to run the Jupyter package due to [this bug](https://github.com/dzop/emacs-jupyter/issues/22)).

Hence, an alternative is to *build from source*:

#### Install essential build tools
```console
sudo apt-get install build-essential
```

#### Get all dependencies (of a previous emacs version)
```console
sudo apt-get build-dep emacs
```
If you run into "`E: Unable to find a source package for emacs`", run `software-properties-gtk` and check the "Source code" checkbox, as discussed [here](https://unix.stackexchange.com/a/436248/173702).

#### Get source
```console
cd ~/Projects
git clone https://github.com/emacs-mirror/emacs.git
```

#### Go to source and checkout desired version
Checkout to a release of your choice (see the repo's [releases](https://github.com/emacs-mirror/emacs/tags)).
```console
cd emacs
git checkout 96dd0196c28bc36779584e47fffcca433c9309cd
```

#### Clean repo if you have built using the same directory before
```console
# Discard stuff from last build
git reset --hard
# Delete all of the last build stuff
git clean -xdf
```

#### Remove conda from `.bashrc`

For some reason, Anaconda breaks Emacs build. Comment out the part that includes conda in `.bashrc` to make the installation process.

#### Build

At this stage, manually install any missing dependencies using apt-get. Some that I had to do myself:

- Missing lib: `autoconf`; Solution: `$ sudo apt install autoconf`
- Missing lib: `gnutls`; Solution: `$ sudo apt install libgnutls-dev`

The `./configure` step accepts arguments. `--with-modules` builds Emacs with dynamics modules support (allowing packages like Jupyter and ZeroMQ); `CC=clang` builds it using the Clang compiler; it was required because building with gcc was not working to use Jupyter, as mentioned before.

```console
./autogen.sh
./configure --with-modules CC=clang
make -j4
```

#### Install

```console
sudo su -
cd .../Projects/emacs
make install
```

The binaries will be installed in `/usr/local/bin/`, which is the default location for programs not managed by the distribution package manager.

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

According to [this tutorial](http://wikemacs.org/wiki/Emacs_server), the best way is to configure it through the init daemon `systemd`. Create a file `~/.config/systemd/user/emacsd.service` with the contents below (note: check whether your Emacs is installed in `/usr/bin` or in `/usr/local/bin` and modify the file accordingly):

```
[Unit]
Description=Emacs: the extensible, self-documenting text editor
Documentation=man:emacs(1) info:Emacs


[Service]
Type=forking
ExecStart=/usr/local/bin/emacs --daemon
ExecStop=/usr/local/bin/emacsclient --eval "(progn (setq kill-emacs-hook nil) (kill-emacs))"
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


# References

1. http://ergoemacs.org/emacs/building_emacs_from_git_repository.html
2. 
