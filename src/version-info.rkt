;; version-info.rkt - provides version info
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(provide get-version
         get-version-name
         get-last-updated)

(define (get-version) "0.0.2")
(define (get-version-name) "Unstable As Frick")
(define (get-last-updated) "2024-03-17")
