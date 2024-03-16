;; input-output.rkt - handles input and output
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(require "version-info.rkt")

(provide read-input
         display-as-output)

(define (read-input)
    (display "Guild >> ")
    (let ((input (string-trim (read-line))))
    (execute-command input)))

(define (display-as-output string)
    (displayln (string-append "-> " string)))

(define (execute-command command)
    (cond
        ((string=? command "exit")
            (display-as-output "Thanks for using Guild!")
            (exit 0))
        ((string=? command "help")
            (display-as-output "Sorry, no help available yet."))
        ((string=? command "version")
            (display-as-output (string-append (get-version) " " (get-version-name))))
        (else
            (display-as-output (string-append "Command not found: " command)))))
