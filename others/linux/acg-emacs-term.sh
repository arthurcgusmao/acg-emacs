#!/bin/bash

# Same as acg-emacs, but open/raise a frame connected to the terminal server (daemon)
# See acg-term.el for more details

emacsclient --socket-name=term -n -e "(null (cl-remove-if (lambda (frame) (null (display-graphic-p frame))) (frame-list)))" | grep >/dev/null nil
if [ "$?" = "1" ]; then
    # no frame open
    emacsclient --socket-name=term -c -n -a "" "$@"
else
    emacsclient --socket-name=term -n -a "" "$@"
fi

# bring application to front
emacsclient --socket-name=term -e '(select-frame-set-input-focus (selected-frame))'

# "right-maximize" window
