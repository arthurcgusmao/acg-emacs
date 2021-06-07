# My configurations for MacOS


## Emacs installation

Same as in for Linux (see [./../linux/README.md](./../linux/README.md)), with the difference that dependencies have to be installed with brew or macports. I use macports, so I installed the dependencies below (taken from [here](https://github.com/jimeh/build-emacs-for-macos)):
```text
autoconf
coreutils
curl
expat
gcc
gmp
gnu-sed
gnutls
jansson
libffi
libgccjit
libiconv
librsvg
libtasn1
libunistring
libxml2
little-cms2
mailutils
make
ncurses
nettle
pkg-config
texinfo
zlib
```

After running `make install`, the output will be placed on a `./nextstep` directory. Move or copy it to the `~/Applications` dir:
```bash
mkdir ~/Applications/Emacs
cp ./nextstep/Emacs.app ~/Applications/Emacs/Emacs.app
```

Then, create symlinks in a directory that is in your shell PATH, so you can call Emacs from the command line. Since I use `~/.opt/bin` for "optional", user-installed binaries, I did:
```bash
ln -s ~/Applications/Emacs/Emacs.app/Contents/MacOS/Emacs ~/.opt/bin/emacs
ln -s ~/Applications/Emacs/Emacs.app/Contents/MacOS/bin/emacsclient ~/.opt/bin/emacsclient
```
