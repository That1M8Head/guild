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

(define (execute-command command)
    (cond
        ;; Let the user exit
        ((string=? command "exit")
            (display-as-output "Thanks for using Guild!")
            (exit 0))
        ;; No help yet
        ((string=? command "help")
            (display-as-output "Sorry, no help available yet."))
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
