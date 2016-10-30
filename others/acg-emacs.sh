#!/bin/bash

emacsclient -n -e "(null (cl-remove-if (lambda (frame) (null (display-graphic-p frame))) (frame-list)))" | grep >/dev/null nil
if [ "$?" = "1" ]; then
    emacsclient -c -n -a "" "$@"
else
    emacsclient -n -a "" "$@"
fi

#emacsclient -a "emacs" "$@" &
