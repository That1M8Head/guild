#!/bin/sh

if command -v racket >/dev/null 2>&1; then
    echo "Running Guild on a sh-system..."
    racket run.rkt
else
    echo "You don't seem to have Racket, or it's not in your PATH."
fi
