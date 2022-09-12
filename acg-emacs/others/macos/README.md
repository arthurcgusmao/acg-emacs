# My configurations for MacOS


## Emacs installation

Same as in for Linux (see [./../linux/README.md](./../linux/README.md)), with the difference that dependencies have to be installed with macports or homebrew. I use homebrew, so I installed the dependencies below (adapted from [here](https://github.com/jimeh/build-emacs-for-macos)):
```bash
brew install autoconf automake coreutils curl expat gcc gmp gnu-sed gnutls jansson libffi libgccjit libiconv librsvg libtasn1 libunistring libxml2 little-cms2 mailutils make ncurses nettle pkg-config texinfo zlib
```

Additionally, install Xcode via the App Store, open the app, accept the license, and run:
```bash
sudo xcode-select --install
```

Then, for the `./configure` step, I used:
```bash
./autogen.sh
./configure --with-xml2 --with-rsvg --with-modules --with-mailutils --with-dbus --with-debug --with-json --with-no-frame-refocus --with-xwidgets --with-cairo --with-native-compilation
```

After running `make install`, the output will be placed on a `./nextstep` directory. Move or copy it to the `~/Applications` dir:
```bash
mkdir ~/Applications/Emacs
cp ./nextstep/Emacs.app ~/Applications/Emacs/Emacs.app
```

Then, add the paths to the executables directory directlly to your shell PATH, so you can call Emacs from the command line. Personally, I put the following in a `.bashrc_extras` file:
```bash
if [ `uname -s` == "Darwin" ]; then
    # Cannot use symlinks because then the application is created without the correct bundle identifier
    export PATH=$HOME"/Applications/Emacs/Emacs.app/Contents/MacOS/bin":$PATH
    export PATH=$HOME"/Applications/Emacs/Emacs.app/Contents/MacOS":$PATH
fi
```

Next, install my custom script for launching or raising an Emacs frame. Since I use `~/.opt/bin` for "optional", user-installed binaries, I did:
```bash
ln -s ~/.config/emacs/acg-emacs/others/macos/acg-emacs.sh ~/.opt/bin/acg-emacs
```

In MacOS, the commands executed by the applescript or the Karabiner will not have your regular PATH values. Thus, you have to manually add it when invoking commands that use one of `emacsclient`, `emacs`, `acg-emacs`, etc.:
```bash
PATH=$HOME/.opt/bin/:$PATH acg-emacs
```

## Package dependencies

Some packages require you to install binaries that would normally be available on Linux but not on MacOS:
```bash
brew install\
    gls      # For Embark
    ripgrep  # For Consult & related
```
