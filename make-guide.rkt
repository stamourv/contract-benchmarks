#lang racket/base

(require "utils.rkt" "guide/guide.rkt")

(call-with-frozen-collects
 "./guide/" '("scribble" "scribblings" "racket/draw" "racket/snip" "pict" "texpict" "mred")
 (lambda () (time (void (make-guide)))))
