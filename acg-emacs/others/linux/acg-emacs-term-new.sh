#!/bin/bash

# Aside from opening/raising the Emacs terminal frame,
# create a new terminal buffer on the current location where the command was run.

./acg-emacs-term.sh -e "(acg/new-term)"
