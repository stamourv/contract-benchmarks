#lang racket/base

(require "utils.rkt")

(call-with-frozen-collects
 "./json/" '("json")
 (lambda ()
   (dynamic-require "json/main.rkt" #f)))