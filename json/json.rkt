#lang racket/base

(require json racket/runtime-path)
(define-runtime-path f "data.json")

(for ([_ (in-range 1000)]) (read-json (open-input-file f)))