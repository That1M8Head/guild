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
    (execute-command input)))

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
                                    (newline)
                                    "Evaluation failed")])
        (let ((result (eval (read (open-input-string expr)) env)))
            (format "~a" result)))))

(define (list-commands)
    (display-as-output "Available commands:")
    (display-as-output "  help - this message")
    (display-as-output "  exit - leave Guild")
    (display-as-output "  version - get the Guild version")
    (display-as-output "Commands wrapped in parentheses are treated as Lisp expressions."))

(define (execute-command command)
    (cond
        ;; Let the user exit
        ((string=? command "exit")
            (display-as-output "Thanks for using Guild!")
            (exit 0))
        ;; Give the user a list of commands
        ((string=? command "help")
            (list-commands))
        ;; Simply display the version
        ((string=? command "version")
            (display-as-output (string-append (get-version) " " (get-version-name))))
        ;; If it's got brackets (parentheses) around the command,
        ;; treat it as an expression and eval it
        ((is-surrounded-by-parens command)
            (display-as-output (evaluate-expression command)))
        ;; Otherwise tell the user it doesn't exist
        (else
            (display-as-output (string-append command " doesn't seem to exist."))
            (display-as-output "If this is a Lisp expression, wrap it in parentheses."))))
