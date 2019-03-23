#lang racket

(require "../tests/01-integers.rkt"
         "../tests/02-immediate.rkt"
         "../tests/03-unary.rkt"
         "../tests/04-binop.rkt"
         "../tests/05-if.rkt"
         "../tests/06-let.rkt"
         "../tests/07-cons.rkt"
         "../tests/08-procedures.rkt"
         "../tests/09-strings.rkt"
         "../tests/10-cc.rkt"

         "compiler.rkt"
         "driver.rkt")

(test-all)
