;; core.rkt - the core of the Guild environment
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(require "startup.rkt")       ;; welcome message and not much else
(require "input-output.rkt")  ;; manages user interactions and program responses

(define (main)
    (welcome-message)
    (let loop ()
        (read-input)
        (loop)))

(main)