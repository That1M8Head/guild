;; version-info.rkt - provides version info
;; Copyright (c) 2024 Arsalan "Velocity" Kazmi
;; This file is part of Guild.

#lang racket

(provide get-version
         get-version-name)

(define (get-version) "0.0.1")
(define (get-version-name) "Unstable As Frick")
