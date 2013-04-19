#lang racket/base
(require ;; (planet williams/science:4:=6/ode-initval)
         "science/4/6/ode-initval.rkt"
         )

;; Change "ode-initval.rkt" to add this contract to `ode-evolve-apply':
#;
  (-> ode-evolve?
      ode-control?
      ode-step?
      ode-system?
      box?
      real?
      box?
      (vectorof real?)
      any)

(define (func t y f params)
  (let ((mu (car params))
        (y0 (vector-ref y 0))
        (y1 (vector-ref y 1)))
    (vector-set! f 0 y1)
    (vector-set! f 1 (- (- y0) (* mu y1 (- (* y0 y0) 1.0))))))

(define (ode-example-2)
  (let* ((type rk4-ode-type)
         (step (make-ode-step type 2))
         (control (control-y-new 1.0e-6 0.0))
         (evolve (make-ode-evolve 2))
         (mu 1.0)
         (system (make-ode-system func #f 2 (list mu)))
         (t (box 0.0))
         (t1 1000.0)
         (h (box 1.0e-6))
         (y (vector 1.0 0.0))
         (y0-values '())
         (y1-values '()))
    (time
     (let loop ()
       (when (< (unbox t) t1)
         (ode-evolve-apply
          evolve control step system
          t t1 h y)
         (set! y0-values (cons (vector (unbox t) (vector-ref y 0)) y0-values))
         (set! y1-values (cons (vector (unbox t) (vector-ref y 1)) y1-values))
         (loop))))
    ;; (printf "Number of iterations   = ~a~n" (ode-evolve-count evolve))
    ;; (printf "Number of failed steps = ~a~n" (ode-evolve-failed-steps evolve))
    ))

(ode-example-2)
