#lang racket/base

(provide slowdown
         fixed-number
         get-numbers
         load-data
         save-data)

(define (save-data file data) 
  (call-with-output-file (build-path "data" file)
    (λ (port) 
      (write data port)
      (newline port))
    'truncate))


(define (load-data file)
  (call-with-input-file (build-path "data" file) read))

(define (slowdown raw1 raw2)
  (map (λ (x y) (list (car x) (/ (cadr x) (cadr y)))) raw1 raw2))

(define (fixed-number num raw1)
  (map (λ (x) (list (car x) num)) raw1))

(define (get-numbers mk f start finish inc)
  (let loop ([n start])
    (cond
      [(n . > . finish)
       (newline)
       '()]
      [else
       (display ".") (flush-output)
       (let ([lst (mk n)])
         (collect-garbage) (collect-garbage) (collect-garbage) (collect-garbage)
         (let-values ([(_ a b c) (time-apply (λ () (f lst)) '())])
           (cons (list n (max 1 (- a c)))
                 (loop (+ n inc)))))])))
