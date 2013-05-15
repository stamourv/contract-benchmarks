#lang racket/base

(require "utils.rkt")

(call-with-frozen-collects
 "./tr/" '("typed" "typed-racket" "typed-scheme")
 (lambda () (time (dynamic-require "tr/new-metrics.rkt" #f))))
