;;; Science Collection
;;; ode-initval/rk4.rkt
;;; Copyright (c) 2004-2011 M. Douglas Williams
;;;
;;; This file is part of the Science Collection.
;;;
;;; The Science Collection is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the License
;;; or (at your option) any later version.
;;;
;;; The Science Collection is distributed in the hope that it will be useful,
;;; but WITHOUT WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with the Science Collection.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;; -------------------------------------------------------------------
;;;
;;; Version  Date      Description
;;; 3.0.1    07/01/08  Added header.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require scheme/unsafe/ops)

(define-values (struct:rk4-state
                rk4-state-constructor
                rk4-state?
                rk4-state-field-ref
                set-rk4-state-field!)
  (make-struct-type 'rk4-state #f 3 0))

(define (make-rk4-state dim)
  (rk4-state-constructor
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)))

(define rk4-state-k
  (make-struct-field-accessor rk4-state-field-ref 0 'k))
(define set-rk4-state-k!
  (make-struct-field-mutator set-rk4-state-field! 0 'k))

(define rk4-state-y0
  (make-struct-field-accessor rk4-state-field-ref 1 'y0))
(define set-rk4-state-y0!
  (make-struct-field-mutator set-rk4-state-field! 1 'y0))

(define rk4-state-ytmp
  (make-struct-field-accessor rk4-state-field-ref 2 'ytmp))
(define set-rk4-state-ytmp!
  (make-struct-field-mutator set-rk4-state-field! 2 'ytmp))

(define (rk4-apply state dim t h y y-err dydt-in dydt-out system)
  (let ((k (rk4-state-k state))
        (y0 (rk4-state-y0 state))
        (ytmp (rk4-state-ytmp state)))
    ;; Copy the starting value.  We will write over the y[] vector,
    ;; using it for scratch and then filling it with the final result.
    (vector-copy! y0 0 y)
    (if dydt-in
        (vector-copy! k 0 dydt-in)
        (ode-system-function-eval system t y0 k))
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       y i (unsafe-fl* (unsafe-fl/ h 6.0) (unsafe-vector-ref k i)))
      (unsafe-vector-set!
       ytmp i (unsafe-fl+ (unsafe-vector-ref y0 i)
                          (unsafe-fl* 0.5 (unsafe-fl* h (unsafe-vector-ref k i))))))
    ;; k2 step
    (ode-system-function-eval system (unsafe-fl+ t (unsafe-fl* 0.5 h)) ytmp k)
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       y i (unsafe-fl+ (unsafe-vector-ref y i)
                       (unsafe-fl* (unsafe-fl/ h 3.0) (unsafe-vector-ref k i))))
      (unsafe-vector-set!
       ytmp i (unsafe-fl+ (unsafe-vector-ref y0 i)
                          (unsafe-fl* 0.5 (unsafe-fl* h (unsafe-vector-ref k i))))))
    ;; k3 step
    (ode-system-function-eval system (unsafe-fl+ t (unsafe-fl* 0.5 h)) ytmp k)
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       y i (unsafe-fl+ (unsafe-vector-ref y i)
                       (unsafe-fl* (unsafe-fl/ h 3.0) (unsafe-vector-ref k i))))
      (unsafe-vector-set!
       ytmp i (unsafe-fl+ (unsafe-vector-ref y0 i) (unsafe-fl* h (unsafe-vector-ref k i)))))
    ;; k4 step, error estimate, and final sum
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       y i (unsafe-fl+ (unsafe-vector-ref y i)
                       (unsafe-fl* (unsafe-fl/ h 6.0) (unsafe-vector-ref k i))))
      (vector-set!
       y-err i (unsafe-fl* h (unsafe-vector-ref y i)))
      (vector-set!
       y i (unsafe-fl+ (unsafe-vector-ref y i) (unsafe-vector-ref y0 i)))
      (when dydt-out
        (unsafe-vector-set! dydt-out i (unsafe-vector-ref k i))))))

(define (rk4-reset state dim)
  (let ((k (rk4-state-k state))
        (y0 (rk4-state-y0 state))
        (ytmp (rk4-state-ytmp state)))
    (for ((i (in-range dim)))
      (unsafe-vector-set! k i 0.0)
      (unsafe-vector-set! y0 i 0.0)
      (unsafe-vector-set! ytmp i 0.0))))

(define (rk4-order state)
  4)

(define rk4-ode-type
  (make-ode-step-type
   "rk4"
   #t
   #f
   make-rk4-state
   rk4-apply
   rk4-reset
   rk4-order))