;;; Science Collection
;;; ode-initval/rk2.rkt
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

(define-values (struct:rk2-state
                rk2-state-constructor
                rk2-state?
                rk2-state-field-ref
                set-rk2-state-field!)
  (make-struct-type 'rk2-state #f 4 0))

(define (make-rk2-state dim)
  (rk2-state-constructor
   (make-vector dim)
   (make-vector dim)
   (make-vector dim)
   (make-vector dim)))

(define rk2-state-k1
  (make-struct-field-accessor rk2-state-field-ref 0 'k1))
(define set-rk2-state-k1!
  (make-struct-field-mutator set-rk2-state-field! 0 'k1))

(define rk2-state-k2
  (make-struct-field-accessor rk2-state-field-ref 1 'k2))
(define set-rk2-state-k2!
  (make-struct-field-mutator set-rk2-state-field! 1 'k2))

(define rk2-state-k3
  (make-struct-field-accessor rk2-state-field-ref 2 'k3))
(define set-rk2-state-k3!
  (make-struct-field-mutator set-rk2-state-field! 2 'k3))

(define rk2-state-ytmp
  (make-struct-field-accessor rk2-state-field-ref 3 'ytmp))
(define set-rk2-state-ytmp!
  (make-struct-field-mutator set-rk2-state-field! 3 'ytmp))

(define (rk2-apply state dim t h y y-err dydt-in dydt-out system)
  (let ((k1 (rk2-state-k1 state))
        (k2 (rk2-state-k2 state))
        (k3 (rk2-state-k3 state))
        (ytmp (rk2-state-ytmp state)))
    ;; k1 step
    (if dydt-in
        (vector-copy! k1 0 dydt-in)
        (ode-system-function-eval system t y k1))
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       ytmp i
       (unsafe-fl+ (unsafe-vector-ref y i)
                   (unsafe-fl* 0.5 (unsafe-fl* h (unsafe-vector-ref k1 i))))))
    ;; k2 step
    (ode-system-function-eval system (+ t (* 0.5 h)) ytmp k2)
    (for ((i (in-range dim)))
      (vector-set!
       ytmp i
       (unsafe-fl+ (unsafe-vector-ref y i)
                   (unsafe-fl* h (unsafe-fl+ (unsafe-fl- 0.0 (unsafe-vector-ref k1 i))
                                             (unsafe-fl* 2.0 (unsafe-vector-ref k2 i)))))))
    ;; k3 step
    (ode-system-function-eval system (+ t h) ytmp k3)
    ;; final sum and error estimate
    (for ((i (in-range dim)))
      (let ((ksum3
             (unsafe-fl/ (unsafe-fl+
                          (unsafe-vector-ref k1 i)
                          (unsafe-fl+
                           (unsafe-fl* 4.0 (unsafe-vector-ref k2 i))
                           (unsafe-vector-ref k3 i)))
                         6.0)))
        (unsafe-vector-set!
         y i
         (unsafe-fl+ (unsafe-vector-ref y i) (unsafe-fl* h ksum3)))
        (unsafe-vector-set!
         y-err i
         (unsafe-fl* h (unsafe-fl- (unsafe-vector-ref k2 i) ksum3)))
        (when dydt-out
          (for ((i (in-range dim)))
            (unsafe-vector-set! dydt-out i ksum3)))))))

(define (rk2-reset state dim)
  (let ((k1 (rk2-state-k1 state))
        (k2 (rk2-state-k2 state))
        (k3 (rk2-state-k3 state))
        (ytmp (rk2-state-ytmp state)))
    (for ((i (in-range dim)))
      (unsafe-vector-set! k1 i 0.0)
      (unsafe-vector-set! k2 i 0.0)
      (unsafe-vector-set! k3 i 0.0)
      (unsafe-vector-set! ytmp i 0.0))))

(define (rk2-order state)
  2)

(define rk2-ode-type
  (make-ode-step-type
   "rk2"
   #t
   #f
   make-rk2-state
   rk2-apply
   rk2-reset
   rk2-order))