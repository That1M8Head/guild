;; input-output.rkt - manages user interactions and program responses
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(require "version-info.rkt")

(provide read-input
         display-as-output)

(define (read-input)
    (display "Guild >> ")
    (let ((input (string-trim (read-line))))
        (cond ((string=? input "")
            (read-input))
            (else
            (execute-command input)))))

(define (display-as-output string)
    (displayln (string-append "-> " string)))

(define (is-surrounded-by-parens str)
    (and (string=? (substring str 0 1) "(")
        (string=? (substring str (- (string-length str) 1)) ")")))

(define (evaluate-expression expr)
    (define (add-imports-to-eval env)
        (define imported-procedures '(+ - * / = < > <= >= abs))
        (for-each (lambda (proc) (eval `(define ,proc ,proc) env)) imported-procedures)
        env)

    (let ((env (add-imports-to-eval (make-base-namespace))))
        (with-handlers ([exn:fail? (lambda (ex)
                                    (display "Evaluation error: ")
                                    (display (exn-message ex))
                                    (display "If you intended to use a built-in command, try (list-commands) for help.")
                                    (newline)
                                    "Evaluation failed")])
        (let ((result (eval (read (open-input-string expr)) env)))
            (format "~a" result)))))

(define (list-commands)
    (display-as-output "Available commands:")
    (display-as-output "  (list-commands) - this message")
    (display-as-output "  (quit-guild) - leave Guild")
    (display-as-output "  (get-version) - get the Guild version")
    (display-as-output "  (get-version-name) - get the name of the Guild version")
    (display-as-output "Commands wrapped in parentheses that don't match the above are treated as Lisp expressions."))

(define (execute-command command)
    (cond
        ((not (is-surrounded-by-parens command))
            (display-as-output (string-append command " is using invalid syntax."))
            (display-as-output "Type (list-commands) for help."))
        ;; Let the user exit
        ((string=? command "(quit-guild)")
            (display-as-output "Thanks for using Guild!")
            (exit 0))
        ;; Give the user a list of commands
        ((string=? command "(list-commands)")
            (list-commands))
        ;; Simply display the version
        ((string=? command "(get-version)")
            (display-as-output (get-version)))
        ;; Or the name
        ((string=? command "(get-version-name)")
            (display-as-output (get-version-name)))
        ;; Or the last update
        ((string=? command "(get-last-updated)")
            (display-as-output (get-last-updated)))
        ;; Otherwise treat it as an expression and eval it
        (else
            (display-as-output (evaluate-expression command)))))
