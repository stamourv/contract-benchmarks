#lang racket
;;; Science Collection
;;; chebyshev.rkt
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
;;; -----------------------------------------------------------------------------
;;;
;;; This code in based on the Chebyshev Approximations in the GNU Scientific
;;; Library (GSL), which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 0.1.0    08/07/04  This is the initial release of the Chebyshev approx-
;;;                    imations routines ported from GSL. (Doug Williams)
;;; 1.0.0    09/29/04  Added contracts for functions.  Marked as ready fo
;;;                    Release 1.0.  (Doug Williams)
;;; 2.0.0    11/17/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme V4.0.  (Doug Williams)
;;; 3.0.0    06/09/08  Global changes for V4.0.  (Doug Williams)
;;; 3.0.1    09/13/09  Rewritten for better clarity and efficiency; changes
;;;                    make-chebyshev-series to make it more general; added
;;;                    derivative and integral series construction. (Doug
;;;                    Williams)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)
;;; 4.0.1    08/12/11  Used new unsafe vector references. (MDW)

(require racket/unsafe/ops)
(require "unsafe-ops-utils.rkt")

;;; (struct chebyshev-series (coefficients order lower upper))
;;;   coefficients : (vectorof real?)
;;;   order : exact-positive-integer?
;;;   lower : real?
;;;   upper : real?
(define-values (struct:chebyshev-series
                construct-chebyshev-series
                chebyshev-series?
                chebyshev-series-ref
                chebyshev-series-set!)
  (make-struct-type 'chebyshev-series #f 4 0))

(define chebyshev-series-coefficients
  (make-struct-field-accessor chebyshev-series-ref 0 'coefficients))
(define set-chebyshev-series-coefficients!
  (make-struct-field-mutator chebyshev-series-set! 0 'coefficients))

(define chebyshev-series-order
  (make-struct-field-accessor chebyshev-series-ref 1 'order))

(define chebyshev-series-lower
  (make-struct-field-accessor chebyshev-series-ref 2 'lower))
(define set-chebyshev-series-lower!
  (make-struct-field-mutator chebyshev-series-set! 2 'lower))
  
(define chebyshev-series-upper
  (make-struct-field-accessor chebyshev-series-ref 3 'upper))
(define set-chebyshev-series-upper!
  (make-struct-field-mutator chebyshev-series-set! 3 'upper))

;;; (make-chebyshev-series order) -> chebyshev-series?
;;;   order : exact-positive-integer?
;;; (make-chebyshev-series coeffs-or-func order lower upper) -> chebyshev-series?
;;;   coeffs-of-func : (or/c (vectorof inexact-real?) (-> real? inexact-real?))
;;;   order : exact-positive-integer?
;;;   lower : real?
;;;   upper : real?
(define make-chebyshev-series
  (case-lambda
    ((order)
     (construct-chebyshev-series (make-vector (add1 order) 0.0) order 0.0 0.0))
    ((coeffs-or-func order lower upper)
     (with-float (lower upper)
       (unless (< lower upper)
         (error 'make-chebyshev-series
                "null interval [~a, ~a]" lower upper))
       (cond ((vector? coeffs-or-func)
              (unless (= (vector-length coeffs-or-func) (add1 order))
                (error 'make-chebyshev-series
                       "expected vector of length ~a, given vector of length ~a"
                       (add1 order) (vector-length coeffs-or-func)))
              (construct-chebyshev-series
               (real-vector->float-vector coeffs-or-func) order lower upper))
             ((procedure? coeffs-or-func)
              (construct-chebyshev-series
               (compute-coefficients coeffs-or-func order lower upper)
               order lower upper))
             (else
              (error 'make-chebyshev-series
                     "expected vector or procedure, given ~a" coeffs-or-func)))))))

;;; (compute-coefficients func order a b) -> (vectorof inexact-real?)
;;;   func : (-> real? real?)
;;;   order : exact-positive-integer?
;;;   a : inexact-real?
;;;   b : inexact-real?
(define (compute-coefficients func order a b)
  (let* ((bma (* 0.5 (- b a)))
         (bpa (* 0.5 (+ b a)))
         (fac (/ 2.0 (add1 order)))
         (f (build-vector
             (add1 order)
             (lambda (i)
               (let ((y (cos (* pi (/ (+ i 0.5) (add1 order))))))
                 (func (+ (* y bma) bpa)))))))
    (build-vector
     (add1 order)
     (lambda (j)
       (* fac (for/fold ((sum 0.0))
                        ((k (in-naturals))
                         (x (in-vector f)))
                (+ sum (* x (cos (/ (* pi j (+ k 0.5)) (add1 order)))))))))))

;;; (make-chebyshev-series-order order) -> chebyshev-series?
;;;   order : exact-positive-integer?
;;; Legacy function to create a new Chebyshev series with the specified order.
;;; Use (make-chebyshev-series order) instead.
(define (make-chebyshev-series-order order)
  (make-chebyshev-series order))

;;; (chebyshev-series-init series func a b) -> void?
;;;   series : chebyshev-series?
;;;   order : exact-positive-integer?
;;;   a : real?
;;;   b : real?
;;; Legacy function to initialize a Chebyshev series to estimate func on the
;;; interval [a, b]. Use (make-chebyshev-series func order a b) instead.
(define (chebyshev-series-init series func a b)
  (with-float (a b)
    (when (>= a b)
      (error 'make-chebyshev-series
             "null interval [~a, ~a]" a b))
    (let ((order (chebyshev-series-order series)))
      (set-chebyshev-series-coefficients!
       series (compute-coefficients func order a b))
      (set-chebyshev-series-lower! series a)
      (set-chebyshev-series-upper! series b))))

;;; (chebyshev-eval series x) -> real?
;;;   series : chebyshev-series?
;;;   x : real?
(define (chebyshev-eval series x)
  (with-float (x)
    (let* ((coefficients (chebyshev-series-coefficients series))
           (order (chebyshev-series-order series))
           (lower (chebyshev-series-lower series))
           (upper (chebyshev-series-upper series))
           (c0 (unsafe-vector-ref coefficients 0))
           (y (unsafe-fl/ (unsafe-fl- (unsafe-fl* 2.0 x)
                                      (unsafe-fl+ lower upper))
                          (unsafe-fl- upper lower)))
           (y2 (unsafe-fl* 2.0 y)))
      (let-values (((d dd)
                    (for/fold ((d 0.0)
                               (dd 0.0))
                              ((c (in-vector coefficients order 0 -1)))
                      (values (unsafe-fl+ (unsafe-fl* y2 d)
                                          (unsafe-fl+ (unsafe-fl- 0.0 dd) c))
                              d))))
        (unsafe-fl+ (unsafe-fl* y d)
                    (unsafe-fl+ (unsafe-fl- 0.0 dd) (unsafe-fl* 0.5 c0)))))))

;;; (chebyshev-eval-n series n x) -> real?
;;;   series : chebyshev-series?
;;;   n : exact-positive-integer?
;;;   x : real?
(define (chebyshev-eval-n series n x)
  (with-float (x)
    (let* ((coefficients (chebyshev-series-coefficients series))
           (order (chebyshev-series-order series))
           (lower (chebyshev-series-lower series))
           (upper (chebyshev-series-upper series))
           (c0 (unsafe-vector-ref coefficients 0))
           (y (unsafe-fl/ (unsafe-fl- (unsafe-fl* 2.0 x)
                                      (unsafe-fl+ lower upper))
                          (unsafe-fl- upper lower)))
           (y2 (unsafe-fl* 2.0 y)))
      (let-values (((d dd)
                    (for/fold ((d 0.0)
                               (dd 0.0))
                              ((c (in-vector coefficients (min n order) 0 -1)))
                      (values (unsafe-fl+ (unsafe-fl* y2 d)
                                          (unsafe-fl+ (unsafe-fl- 0.0 dd) c))
                              d))))
        (unsafe-fl+ (unsafe-fl* y d)
                    (unsafe-fl+ (unsafe-fl- 0.0 dd) (unsafe-fl* 0.5 c0)))))))

;;; (make-chebyshev-series-derivative series) -> chebyshev-series?
;;;   series : chebyshev-series?
(define (make-chebyshev-series-derivative series)
  (let* ((series-coefficients (chebyshev-series-coefficients series))
         (order (chebyshev-series-order series))
         (lower (chebyshev-series-lower series))
         (upper (chebyshev-series-upper series))
         (n (add1 order))
         (deriv-coefficients (make-vector n))
         (con (/ 2.0 (- upper lower))))
    (vector-set! deriv-coefficients (sub1 n) 0.0)
    (when (> n 1)
      (vector-set! deriv-coefficients (- n 2)
                   (* 2.0 (- n 1) (vector-ref series-coefficients (- n 1))))
      (for ((i (in-range (- n 3) 0 -1)))
        (vector-set! deriv-coefficients i
                     (+ (vector-ref deriv-coefficients (+ i 2))
                        (* 2.0 (+ i 1) (vector-ref series-coefficients (+ i 1))))))
      (vector-set! deriv-coefficients 0
                   (+ (vector-ref deriv-coefficients 2)
                      (* 2.0 (vector-ref series-coefficients 1))))
      (for ((i (in-range n)))
        (vector-set! deriv-coefficients i
                     (* (vector-ref deriv-coefficients i) con))))
    (make-chebyshev-series deriv-coefficients order lower upper)))

;;; (make-chebyshev-series-integral series) -> chebyshev-series?
;;;   series : chebyshev-series?
(define (make-chebyshev-series-integral series)
  (let* ((series-coefficients (chebyshev-series-coefficients series))
         (order (chebyshev-series-order series))
         (lower (chebyshev-series-lower series))
         (upper (chebyshev-series-upper series))
         (n (add1 order))
         (integ-coefficients (make-vector n))
         (con (* 0.25 (- upper lower))))
    (cond ((= n 1)
           (vector-set! integ-coefficients 0 0.0))
          ((= n 2)
           (vector-set! integ-coefficients 1
                        (* con (vector-ref series-coefficients 0)))
           (vector-set! integ-coefficients 0
                        (* 2.0 (vector-ref integ-coefficients 1))))
          (else
           (let-values (((sum fac)
                         (for/fold ((sum 0.0)
                                    (fac 1.0))
                                   ((i (in-range 1 (- n 2))))
                           (vector-set! integ-coefficients i
                                        (* con (- (vector-ref series-coefficients (- i 1))
                                                  (/ (vector-ref series-coefficients (+ 1))
                                                     i))))
                           (values (+ sum (* fac (vector-ref integ-coefficients i)))
                                   (- fac)))))
             (vector-set! integ-coefficients (- n 1)
                          (* con (/ (vector-ref series-coefficients (- n 2))
                                    (- n 1))))
             (set! sum (+ sum (* fac (vector-ref integ-coefficients (- n 1)))))
             (vector-set! integ-coefficients 0
                          (* 2.0 sum)))))
    (make-chebyshev-series integ-coefficients order lower upper)))

;;; Module Contracts

(provide
 (rename-out (chebyshev-eval unchecked-chebyshev-eval)
             (chebyshev-eval-n unchecked-chebyshev-eval-n)))

(provide/contract
 (chebyshev-series?
  (-> any/c boolean?))
 (chebyshev-series-coefficients
  (-> chebyshev-series? (vectorof inexact-real?)))
 (chebyshev-series-order
  (-> chebyshev-series? exact-positive-integer?))
 (chebyshev-series-lower
  (-> chebyshev-series? inexact-real?))
 (chebyshev-series-upper
  (-> chebyshev-series? inexact-real?))
 (make-chebyshev-series
  (case-> (-> exact-positive-integer? chebyshev-series?)
          (-> (or/c (vectorof real?) (-> real? real?)) exact-positive-integer?
              real? real? chebyshev-series?)))
 (make-chebyshev-series-order
  (-> exact-positive-integer? chebyshev-series?))
 (chebyshev-series-init
  (-> chebyshev-series? (-> real? real?) real? real? void?))
 (chebyshev-eval
  (-> chebyshev-series? real? real?))
 (chebyshev-eval-n
  (-> chebyshev-series? exact-positive-integer? real? real?))
 (make-chebyshev-series-derivative
  (-> chebyshev-series? chebyshev-series?))
 (make-chebyshev-series-integral
  (-> chebyshev-series? chebyshev-series?)))