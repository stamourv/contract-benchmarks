#lang racket/base

(require "utils.rkt")

(call-with-frozen-collects
 "./slideshow/" '("slideshow" "pict" "racket/draw")
 (lambda () (dynamic-require "slideshow/main.rkt" #f)))
