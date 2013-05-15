#lang racket/base

(require "utils.rkt" "guide/guide.rkt")

(call-with-frozen-collects
 "./guide/" '("scribble" "scribblings")
 (lambda () (render-guide)))
