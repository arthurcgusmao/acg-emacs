#!/bin/bash

# Differently than in Linux, in MacOS we have to use the emacs executable
# placed in the application content instead of symlinks because otherwise the
# app is created without the correct bundle identifiers (e.g., karabiner cannot
# properly filter which application is raised)
export PATH=$HOME"/Applications/Emacs/Emacs.app/Contents/MacOS/bin":$PATH
export PATH=$HOME"/Applications/Emacs/Emacs.app/Contents/MacOS":$PATH

emacsclient -n -e "(null (cl-remove-if (lambda (frame) (null (display-graphic-p frame))) (frame-list)))" | grep >/dev/null nil
if [ "$?" = "1" ]; then
    # No frame open
    emacsclient -c -n -a "" "$@"
else
    emacsclient -n -a "" "$@"
fi

# Set focus and bring application to front
emacsclient -e '(select-frame-set-input-focus (selected-frame))' && emacsclient -e '(raise-frame (selected-frame))'
