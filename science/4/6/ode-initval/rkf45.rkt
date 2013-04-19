;;; Science Collection
;;; ode-initval/rkf45.rkt
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

(define ah (vector (/ 1.0 4.0)
                   (/ 3.0 8.0)
                   (/ 12.0 13.0)
                   1.0
                   (/ 1.0 2.0)))

(define b3 (vector (/ 3.0 32.0)
                   (/ 9.0 32.0)))
(define b4 (vector (/  1932.0 2197.0)
                   (/ -7200.0 2197.0)
                   (/  7296.0 2197.0)))
(define b5 (vector (/   8341.0 4104.0)
                   (/ -32832.0 4104.0)
                   (/  29440.0 4104.0)
                   (/   -845.0 4104.0)))
(define b6 (vector (/  -6080.0 20520.0)
                   (/  41040.0 20520.0)
                   (/ -28352.0 20520.0)
                   (/   9295.0 20520.0)
                   (/  -5643.0 20520.0)))

(define c1 (/   902880.0 7618050.0))
(define c3 (/  3953664.0 7618050.0))
(define c4 (/  3855735.0 7618050.0))
(define c5 (/ -1371249.0 7618050.0))
(define c6 (/   277020.0 7618050.0))

(define ec (vector 0.0
                   (/ 1.0 360.0)
                   0.0
                   (/ -128.0 4275.0)
                   (/ -2197.0 75240.0)
                   (/ 1.0 50.0)
                   (/ 2.0 55.0)))

(define-values (struct:rkf45-state
                rkf45-state-constructor
                rkf45-state?
                rkf45-state-field-ref
                set-rkf45-state-field!)
  (make-struct-type 'rkf45-state #f 8 0))

(define (make-rkf45-state dim)
  (rkf45-state-constructor
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)))

(define rkf45-state-k1
  (make-struct-field-accessor rkf45-state-field-ref 0 'k1))
(define set-rkf45-state-k1!
  (make-struct-field-mutator set-rkf45-state-field! 0 'k1))

(define rkf45-state-k2
  (make-struct-field-accessor rkf45-state-field-ref 1 'k2))
(define set-rkf45-state-k2!
  (make-struct-field-mutator set-rkf45-state-field! 1 'k2))

(define rkf45-state-k3
  (make-struct-field-accessor rkf45-state-field-ref 2 'k3))
(define set-rkf45-state-k3!
  (make-struct-field-mutator set-rkf45-state-field! 2 'k3))

(define rkf45-state-k4
  (make-struct-field-accessor rkf45-state-field-ref 3 'k4))
(define set-rkf45-state-k4!
  (make-struct-field-mutator set-rkf45-state-field! 3 'k4))

(define rkf45-state-k5
  (make-struct-field-accessor rkf45-state-field-ref 4 'k5))
(define set-rkf45-state-k5!
  (make-struct-field-mutator set-rkf45-state-field! 4 'k5))

(define rkf45-state-k6
  (make-struct-field-accessor rkf45-state-field-ref 5 'k6))
(define set-rkf45-state-k6!
  (make-struct-field-mutator set-rkf45-state-field! 5 'k6))

(define rkf45-state-y0
  (make-struct-field-accessor rkf45-state-field-ref 6 'y0))
(define set-rkf45-state-y0!
  (make-struct-field-mutator set-rkf45-state-field! 6 'y0))

(define rkf45-state-ytmp
  (make-struct-field-accessor rkf45-state-field-ref 7 'ytmp))
(define set-rkf45-state-ytmp!
  (make-struct-field-mutator set-rkf45-state-field! 7 'ytmp))

(define (rkf45-apply state dim t h y y-err dydt-in dydt-out system)
  (let ((k1 (rkf45-state-k1 state))
        (k2 (rkf45-state-k2 state))
        (k3 (rkf45-state-k3 state))
        (k4 (rkf45-state-k4 state))
        (k5 (rkf45-state-k5 state))
        (k6 (rkf45-state-k6 state))
        (ytmp (rkf45-state-ytmp state)))
    ;; k1 step
    (if dydt-in
        (vector-copy! k1 0 dydt-in)
        (ode-system-function-eval system t y k1))
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       ytmp i
       (unsafe-fl+ (unsafe-vector-ref y i)
                   (unsafe-fl* (unsafe-vector-ref ah 0)
                               (unsafe-fl* h
                                           (unsafe-vector-ref k1 i))))))
    ;; k2 step
    (ode-system-function-eval system (unsafe-fl+ t (unsafe-fl* (unsafe-vector-ref ah 0) h)) ytmp k2)
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       ytmp i
       (unsafe-fl+ (unsafe-vector-ref y i)
                   (unsafe-fl* h
                               (unsafe-fl+ (unsafe-fl* (unsafe-vector-ref b3 0)
                                                       (unsafe-vector-ref k1 i))
                                           (unsafe-fl* (unsafe-vector-ref b3 1)
                                                       (unsafe-vector-ref k2 i)))))))
    ;; k3 step
    (ode-system-function-eval system (unsafe-fl+ t (unsafe-fl* (unsafe-vector-ref ah 1) h)) ytmp k3)
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       ytmp i
       (unsafe-fl+ (unsafe-vector-ref y i)
                   (unsafe-fl* h
                               (unsafe-fl+ (unsafe-fl* (unsafe-vector-ref b4 0)
                                                       (unsafe-vector-ref k1 i))
                                           (unsafe-fl+
                                            (unsafe-fl* (unsafe-vector-ref b4 1)
                                                        (unsafe-vector-ref k2 i))
                                            (unsafe-fl* (unsafe-vector-ref b4 2)
                                                        (unsafe-vector-ref k3 i))))))))
    ;; k4 step
    (ode-system-function-eval system (unsafe-fl+ t (unsafe-fl* (unsafe-vector-ref ah 2) h)) ytmp k4)
    (for ((i (in-range dim)))
      (vector-set!
       ytmp i
       (unsafe-fl+ (unsafe-vector-ref y i)
                   (unsafe-fl* h
                               (unsafe-fl+ 
                                (unsafe-fl* (unsafe-vector-ref b5 0)
                                            (unsafe-vector-ref k1 i))
                                (unsafe-fl+
                                 (unsafe-fl* (unsafe-vector-ref b5 1)
                                             (unsafe-vector-ref k2 i))
                                 (unsafe-fl+ 
                                  (unsafe-fl* (unsafe-vector-ref b5 2)
                                              (unsafe-vector-ref k3 i))
                                  (unsafe-fl* (unsafe-vector-ref b5 3)
                                              (unsafe-vector-ref k4 i)))))))))
    ;; k5 step
    (ode-system-function-eval system (unsafe-fl+ t (unsafe-fl* (unsafe-vector-ref ah 3) h)) ytmp k5)
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       ytmp i
       (unsafe-fl+ (unsafe-vector-ref y i)
                   (unsafe-fl* h
                               (unsafe-fl+
                                (unsafe-fl* (unsafe-vector-ref b6 0)
                                            (unsafe-vector-ref k1 i))
                                (unsafe-fl+
                                 (unsafe-fl* (unsafe-vector-ref b6 1)
                                             (unsafe-vector-ref k2 i))
                                 (unsafe-fl+
                                  (unsafe-fl* (unsafe-vector-ref b6 2)
                                              (unsafe-vector-ref k3 i))
                                  (unsafe-fl+
                                   (unsafe-fl* (unsafe-vector-ref b6 3)
                                               (unsafe-vector-ref k4 i))
                                   (unsafe-fl* (unsafe-vector-ref b6 4)
                                               (unsafe-vector-ref k5 i))))))))))
    ;; k6 step
    (ode-system-function-eval system (unsafe-fl+ t (unsafe-fl* (unsafe-vector-ref ah 4) h)) ytmp k6)
    (for ((i (in-range dim)))
      (let ((d-i (unsafe-fl+
                  (unsafe-fl* c1 (unsafe-vector-ref k1 i))
                  (unsafe-fl+
                   (unsafe-fl* c3 (unsafe-vector-ref k3 i))
                   (unsafe-fl+
                    (unsafe-fl* c4 (unsafe-vector-ref k4 i))
                    (unsafe-fl+
                     (unsafe-fl* c5 (unsafe-vector-ref k5 i))
                     (unsafe-fl* c6 (unsafe-vector-ref k6 i))))))))
        (unsafe-vector-set!
         y i 
         (unsafe-fl+ (unsafe-vector-ref y i)
                     (unsafe-fl* h d-i)))
        (when dydt-out
          (unsafe-vector-set! dydt-out i d-i))))
    ;; difference between 4th and 5th order
    (for ((i (in-range dim)))
      (unsafe-vector-set!
       y-err i
       (unsafe-fl* h (unsafe-fl+
                      (unsafe-fl* (unsafe-vector-ref ec 1)
                                  (unsafe-vector-ref k1 i))
                      (unsafe-fl+
                       (unsafe-fl* (unsafe-vector-ref ec 3)
                                   (unsafe-vector-ref k3 i))
                       (unsafe-fl+
                        (unsafe-fl* (unsafe-vector-ref ec 4)
                                    (unsafe-vector-ref k4 i))
                        (unsafe-fl+
                         (unsafe-fl* (unsafe-vector-ref ec 5)
                                     (unsafe-vector-ref k5 i))
                         (unsafe-fl* (unsafe-vector-ref ec 6)
                                     (unsafe-vector-ref k6 i)))))))))))

(define (rkf45-reset state dim)
  (let ((k1 (rkf45-state-k1 state))
        (k2 (rkf45-state-k2 state))
        (k3 (rkf45-state-k3 state))
        (k4 (rkf45-state-k4 state))
        (k5 (rkf45-state-k5 state))
        (k6 (rkf45-state-k6 state))
        (ytmp (rkf45-state-ytmp state)))
    (for ((i (in-range dim)))
      (unsafe-vector-set! k1 i 0.0)
      (unsafe-vector-set! k2 i 0.0)
      (unsafe-vector-set! k3 i 0.0)
      (unsafe-vector-set! k4 i 0.0)
      (unsafe-vector-set! k5 i 0.0)
      (unsafe-vector-set! k6 i 0.0)
      (unsafe-vector-set! ytmp i 0.0))))

(define (rkf45-order state)
  5)

(define rkf45-ode-type
  (make-ode-step-type
   "rkf45"
   #t
   #f
   make-rkf45-state
   rkf45-apply
   rkf45-reset
   rkf45-order))
