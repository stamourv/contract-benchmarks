#lang racket/base

(require compiler/compiler)

(provide (all-defined-out))

;; run thunk with frozen collects base/collect1, base/collect2, etc.
(define (call-with-frozen-collects base collects thunk)
  (parameterize
      ([current-library-collection-paths
        (cons (path->complete-path (string->path base))
              (current-library-collection-paths))])
    ;; raco make on the driver won't compile frozen collects, do it explicitly
    (for-each compile-collection-zos collects)
    (thunk)))
