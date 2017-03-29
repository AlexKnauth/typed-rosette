#lang info

(define collection 'multi)

(define deps
  '(("racket" #:version "6.6")
    "base"
    "rosette"
    "turnstile"
    "syntax-classes-lib"
    "rackunit-lib"
    ))

(define build-deps
  '("rackunit-lib"))
