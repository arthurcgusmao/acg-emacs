#!/bin/bash

emacsclient -n -e "(null (cl-remove-if (lambda (frame) (null (display-graphic-p frame))) (frame-list)))" | grep >/dev/null nil
if [ "$?" = "1" ]; then
    # No frame open
    emacsclient -c -n -a "" "$@"
else
    emacsclient -n -a "" "$@"
fi

# Set focus and bring application to front
emacsclient -e '(select-frame-set-input-focus (selected-frame))' && emacsclient -e '(raise-frame (selected-frame))'
