#lang racket/base

(require "utils.rkt" "guide/guide.rkt")

(call-with-frozen-collects
 "./guide/" '("scribble" "scribblings" "racket/draw" "racket/snip" "pict" "texpict" "mred" "at-exp")
 (lambda () (time (void (make-guide)))))
