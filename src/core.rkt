;; core.rkt - the core of the Guild environment
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(define (get-version)
    "0.0.1")

;; This function should go into a separate file, but this is fine for now
(define (welcome-message)
    (displayln "Welcome to Guild!")
    (displayln (string-append "You're using Guild version " (get-version) "."))
    (flush-output))

(define (execute-command command)
    (cond
        ((string=? command "exit")
            (display-as-output "Thanks for using Guild!")
            (exit 0))
        (else
            (display-as-output (string-append "Command not found: " command)))))

(define (read-input)
    (display "Guild >> ")
    (let ((input (string-trim (read-line))))
    (execute-command input)))

(define (display-as-output string)
    (displayln (string-append "-> " string)))

(define (main)
    (welcome-message)
    (display-as-output "Hello, World!")
    (let loop ()
        (read-input)
        (loop)))

(main)