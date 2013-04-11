#lang racket/base

;; measures operations on a binomial heap, driven by a vision algorithm
;; exercises struct contracts

(require racket/port)

(time
 (parameterize ([current-directory "lazy"]
                [current-command-line-arguments
                 `#("koala-face.trace.gz" ; only one of the images
                    "opt chap" ; we use the chaperone version
                    "1")]
                [current-output-port (open-output-nowhere)])
   (dynamic-require "pff.rkt" #f)))
