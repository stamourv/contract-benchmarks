#lang racket

(require compiler/compiler)

(parameterize
    ([current-library-collection-paths
      (cons (path->complete-path (string->path "./tr/"))
            (current-library-collection-paths))])
  ;; raco make on the driver won't compile the frozen TR, so do it explicitly
  (for-each compile-collection-zos '("typed" "typed-racket" "typed-scheme"))
  (time (dynamic-require "tr/new-metrics.rkt" #f)))
