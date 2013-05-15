#lang racket
(provide make-guide render-guide)

(define (make-guide)
  (dynamic-require '(lib "scribblings/guide/guide.scrbl") 'doc))

(define (render-guide)
  (define doc    (make-guide))
  (define render (dynamic-require '(lib "scribble/render") 'render))
  (time (render (list doc) (list "file") #:dest-dir "/tmp")))
