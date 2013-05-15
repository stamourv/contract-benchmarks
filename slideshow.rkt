#lang racket/base

(require "utils.rkt")

(call-with-frozen-collects
 "./slideshow/" '("slideshow" "pict")
 (lambda () (dynamic-require "slideshow/main.rkt" #f)))
