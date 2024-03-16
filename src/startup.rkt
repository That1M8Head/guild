;; startup.rkt - welcome message and not much else
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(require "version-info.rkt")

(provide welcome-message)

(define (welcome-message)
    (displayln "Welcome to Guild!")
    (display "You're using Guild version ")
    (display (get-version))
    (displayln (string-append " (" (get-version-name) ")"))
    (displayln "Type some commands or a Lisp expression.")
    (flush-output))
