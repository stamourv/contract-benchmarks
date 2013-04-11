#lang racket/base
(require plot
         racket/runtime-path
         racket/set)

(define-runtime-path results "results")

(define benchmarks (set))
(define ways-run (set))
(define baseline "none")

(define averaged-table (make-hash))
(define mem-table (make-hash))

;; -> pict
;; builds the plot
;; pre: run load-benchmark-results
(define (build-plot label mem/time)
  (define colors '("red" "green" "blue" "orange" "purple"))
  (plot
   #:y-label label
   (for/list ([benchmark (in-list (sort (set->list benchmarks) string<=?))]
              [n (in-naturals)])
     (discrete-histogram
      #:x-min (* n (set-count ways-run))
      #:color (list-ref colors (modulo n (length colors)))
      #:label benchmark
      (filter
       values
       (for/list ([way-run (in-list (sort (set->list (set-remove ways-run baseline)) string<=?))])
         (and (not (member (adjust-name way-run) '("c" "n")))
              (vector (adjust-name way-run)
                      (/ (hash-ref averaged-table (list benchmark way-run mem/time))
                         (hash-ref averaged-table (list benchmark baseline mem/time)))))))))))

(define (adjust-name b)
  (apply string (map (λ (x) (string-ref x 0)) (regexp-split #rx" " b))))

;; -> void
;; initializes benchmarks, ways-run, and averaged-table
(define (load-results)
  (define unaveraged-table (make-hash))
  
  (for ([x (in-list (directory-list results))])
    (define m (regexp-match #rx"[.](mem|time)[.]" (path->string x)))
    (when m
      (define name (regexp-replace #rx"[.](mem|time)[.][0-9]+$" (path->string x) ""))
      (define mem/time (list-ref m 1))
      (set! benchmarks (set-add benchmarks name))
      (define data (call-with-input-file (build-path results x) read))
      (for ([datum (in-list data)])
        (define which (list-ref datum 0))
        (define time (list-ref datum 1))
        (define key (list name which mem/time))
        (set! ways-run (set-add ways-run which))
        (hash-set! unaveraged-table key (cons time (hash-ref unaveraged-table key '()))))))

  (for ([(k v) (in-hash unaveraged-table)])
    (hash-set! averaged-table k (round (/ (apply + v) (length v))))))

(load-results)
(build-plot "slowdown over no contract checking" "time")
(build-plot "peak memory use, factor over no contract checking" "mem")
