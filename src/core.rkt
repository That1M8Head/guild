;; core.rkt - the core of the Guild environment
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(require "startup.rkt")
(require "input-output.rkt")

(define (main)
    (welcome-message)
    (let loop ()
        (read-input)
        (loop)))

(main)