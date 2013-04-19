#lang racket
;;; Racket Science Collection
;;; math.rkt
;;; Copyright (c) 2004-2011 M. Douglas Williams
;;;
;;; This file is part of the Racket Science Collection.
;;;
;;; The Racket Science Collection is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the License or
;;; (at your option) any later version.
;;;
;;; The Racket Science Collection is distributed in the hope that it will be
;;; useful, but WITHOUT WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with the Racket Science Collection.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;; -----------------------------------------------------------------------------
;;;
;;; This code is based on the Mathematical Functions in the GNU
;;; Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 0.1.0    08/13/04  This is the initial realease of the mathematical function
;;;                    routines ported from GSL. (MDW)
;;; 1.0.0    09/28/04  Removed the code for small integer powers. Marked as ready
;;;                    for Release 1.0. Added contracts for functions (MDW)
;;; 2.0.0    11/17/07  Added unchecked versions and preparing for PLT Scheme
;;;                    V4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 3.1.0    10/04/09  Added use of unsafe operations. Added real->float to
;;;                    protect unsafe operations. Added log10 and dB. Removed
;;;                    the unchecked operations. (MDW)
;;; 4.0.0    05/12/10  Changed the header and restructured the code. Changed
;;;                    the infinite? to return a boolean and added infinite for
;;;                    the old functionality. (MDW)
;;; 4.0.1    05/16/10  Moved unsafe ops utility function to unsafe-ops-utils.ss
;;;                    and expanded the use of unsafe operators. (MDW)

(require scheme/unsafe/ops)
(require "unsafe-ops-utils.rkt")
(require "machine.rkt")

;;; -----------------------------------------------------------------------------
;;;
;;; Mathematical Constants
;;;
;;; The library ensures that the standard BSD mathematical constants are defined.

;;; The base of exponentials, e
(define e 2.71828182845904523536028747135)

;;; The base-2 logarithm of e, log_2(e)
(define log2e 1.44269504088896340735992468100)

;;; The base-10 logarithm of e, log_10(e)
(define log10e 0.43429448190325182765112891892)

;;; The square root of two, sqrt(2)
(define sqrt2 1.41421356237309504880168872421)

;;; The square root of one half, sqrt(1/2)
(define sqrt1/2 0.70710678118654752440084436210)

;;; The square root of three, sqrt(3)
(define sqrt3 1.73205080756887729352744634151)

;;; The constants pi, 2*pi, and 4*pi
;(define pi 3.14159265358979323846264338328)
(define 2*pi (* 2.0 pi))
(define 4*pi (* 4.0 pi))

;;; Pi divided by two, pi/2
(define pi/2 1.57079632679489661923132169164)

;;; Pi divided by four, pi/4
(define pi/4 0.78539816339744830966156608458)

;;; The square root of pi, sqrt(pi)
(define sqrtpi 1.77245385090551602729816748334)

;;; Two divided by the square root of pi, 2/sqrt(pi)
(define 2/sqrtpi 1.12837916709551257389615890312)

;;; The reciprocal of pi, 1/pi
(define 1/pi 0.31830988618379067153776752675)

;;; Twice the reciprocal of pi, 2/pi
(define 2/pi 0.63661977236758134307553505349)

;;; The natural logarithm of ten, ln(10), and its inverse
(define ln10 2.30258509299404568401799145468)
(define 1/ln10 (/ ln10))

;;; The natural logarithm of two, ln(2), and its inverse
(define ln2 0.69314718055994530941723212146)
(define 1/ln2 (/ ln2))

;;; The natural logarithm of pi, ln(pi)
(define lnpi 1.14472988584940017414342735135)

;;; Euler's constant, gamma
(define euler 0.57721566490153286060651209008)

;;; -----------------------------------------------------------------------------
;;;
;;; Infinities and Not-a-Number
;;;
;;; PLT Scheme provides +inf.0 (infinity), -inf.0 (negative infinity), +nan.0
;;; (not a number) and -nan.0 (same as +nan.0) as inexact numerical constants.

;;; (nan? x) -> boolean?
;;;   x: any/c
;;; Returns #t if x is equivalent to +nan.0.
(define (nan? x)
  (eqv? x +nan.0))

;;; (infinity x) -> (or/c -1 1 #f)
;;;   x : any/c
;;; Returns 1 if x is +inf.0, -1 if x is -inf.0, and #f otherwise.
(define (infinite x)
  (cond ((eqv? x +inf.0)
         1)
        ((eqv? x -inf.0)
         -1)
        (else
         #f)))

;;; (infinite? x) -> boolean?
;;;  x : any/c
;;; Returns #t if x is +inf.0 or -inf.0.
(define (infinite? x)
  (if (infinite x) #t #f))

;;; (finite? x) -> boolean?
;;;   x : real?
;;; Returns #t of x is a finite, real number.
(define (finite? x)
  (and (real? x)
       (not (nan? x))
       (not (infinite? x))))

;;; -----------------------------------------------------------------------------
;;;
;;; Elementary Functions

;;; (log1p x) -> number?
;;;   x : real?
;;; Computes the value of log(1+x) is a way that is accurate for small x.
(define (log1p x)
  (with-float (x)
    (let ((y (unsafe-fl+ 1.0 x)))
      (unsafe-fl- (unsafe-fllog y)
                  (unsafe-fl/ (unsafe-fl- (unsafe-fl- y 1.0) x) y)))))

;;; (expm1 x) -> inexact-real?
;;;   x : real?
;;; Computes the value of exp(x)-1 in a way that is accurate for
;;; small x.
(define (expm1 x)
  (with-float (x)
    (if (unsafe-fl< (unsafe-flabs x) ln2)
        ;; Compute the taylor series S = x + (1/2!) x^2 + (1/3!) x^3 +
        ;; ...
        (let ((i 1.0)
              (sum x)
              (term (unsafe-fl/ x 1.0)))
          (let loop ()
            (set! i (unsafe-fl+ i 1.0))
            (set! term (unsafe-fl* term (unsafe-fl/ x i)))
            (set! sum (unsafe-fl+ sum term))
            (when (unsafe-fl> (unsafe-flabs term)
                              (unsafe-fl* (unsafe-flabs sum) double-epsilon))
              (loop)))
          sum)
        (unsafe-fl- (unsafe-flexp x) 1.0))))

;;; (hypot x y) -> inexact-real?
;;;   x : real?
;;;   y : real?
;;; Computes the value of sqrt(x^2 + y^2) in a way that avoids overflow.
(define (hypot x y)
  (with-float (x y)
    (let ((xabs (unsafe-flabs x))
          (yabs (unsafe-flabs y))
          (min 0.0)
          (max 0.0))
      (if (unsafe-fl< xabs yabs)
          (begin
            (set! min xabs)
            (set! max yabs))
          (begin
            (set! min yabs)
            (set! max xabs)))
      (if (unsafe-fl= min 0.0)
          max
          (let ((u (unsafe-fl/ min max)))
            (unsafe-fl* max (unsafe-flsqrt
                             (unsafe-fl+ 1.0 (unsafe-fl* u u)))))))))

;;; (acosh x) -> inexact-real?
;;;   x : real?
;;; Computes the value of arccosh(x).
(define (acosh x)
  (with-float (x)
    (cond ((unsafe-fl> x (unsafe-fl/ 1.0 sqrt-double-epsilon))
           (unsafe-fl+ (unsafe-fllog x) ln2))
          ((unsafe-fl> x 2.0)
           (log (unsafe-fl-
                 (unsafe-fl* 2.0 x)
                 (unsafe-fl/ 1.0
                             (unsafe-fl+
                              (unsafe-flsqrt (unsafe-fl- (unsafe-fl* x x) 1.0))
                              x)))))
          ((unsafe-fl> x 1.0)
           (let ((t (unsafe-fl- x 1.0)))
             (log1p (unsafe-fl+ (unsafe-fl* 2.0 t) (unsafe-fl* t t)))))
          ((unsafe-fl= x 1.0)
           0.0)
          (else
           +nan.0))))

;;; (asinh x) -> inexact-real?
;;;   x : real?
;;; Computes the value of arcsinh(x).
(define (asinh x)
  (with-float (x)
    (let ((a (unsafe-flabs x))
          (s (if (unsafe-fl< x 0.0) -1.0 1.0)))
      (cond ((unsafe-fl> a (unsafe-fl/ 1.0 sqrt-double-epsilon))
             (unsafe-fl* s (unsafe-fl+ (unsafe-fllog a) ln2)))
            ((unsafe-fl> a 2.0)
             (unsafe-fl* s (unsafe-fllog
                            (unsafe-fl+
                             (unsafe-fl* 2.0 a)
                             (unsafe-fl/
                              a
                              (unsafe-flsqrt
                               (unsafe-fl+ (unsafe-fl* a a) 1.0)))))))
            ((unsafe-fl> a sqrt-double-epsilon)
             (let ((a2 (unsafe-fl* a a)))
               (unsafe-fl*
                s
                (log1p (unsafe-fl+
                        a
                        (unsafe-fl/
                         a2
                         (unsafe-fl+
                          1.0
                          (unsafe-flsqrt (unsafe-fl+ 1.0 a2)))))))))
            (else
             x)))))

;;; (atanh x) -> inexact-real?
;;;   x : real?
;;; Computes the value of arctanh(x).
(define (atanh x)
  (with-float (x)
    (let ((a (unsafe-flabs x))
          (s (if (unsafe-fl< x 0.0) -1.0 1.0)))
      (cond ((unsafe-fl> a 1.0)
             +nan.0)
            ((unsafe-fl= a 1.0)
             (if (unsafe-fl< x 0.0) -inf.0 +inf.0))
            ((unsafe-fl>= x 0.5)
             (unsafe-fl* s
                         (unsafe-fl* 0.5
                                     (log1p (unsafe-fl/
                                             (unsafe-fl* 2.0 a)
                                             (unsafe-fl- 1.0 a))))))
            ((unsafe-fl> a double-epsilon)
             (unsafe-fl* s
                         (unsafe-fl*
                          0.5
                          (log1p (unsafe-fl+ (unsafe-fl* 2.0 a) 
                                             (unsafe-fl/
                                              (unsafe-fl* 2.0
                                                          (unsafe-fl* a a))
                                              (unsafe-fl- 1.0 a)))))))
            (else
             x)))))

;;; (ldexp x e) -> inexact-real?
;;;   x : real?
;;;   r : integer?
;;; Computes the value of x * 2^e.
(define (ldexp x e)
  (with-float (x e)
    (let ((p2 (expt 2.0 e)))
      (unsafe-fl* x p2))))

;;; (frexp x) -> inexact-real? integer?
;;; Splits the number x into its normalized fraction f and exponent
;;; e, such that x = f * 2^e and 0.5 <= f < 1.
(define (frexp x)
  (with-float (x)
    (if (unsafe-fl= x 0.0)
        (values
         0.0
         0)
        (let* ((ex (unsafe-flceiling
                    (unsafe-fllog (unsafe-fl/ (unsafe-flabs x) ln2))))
               (ei (inexact->exact ex))
               (f (ldexp x (- ei))))
          (let loop ()
            (when (unsafe-fl>= (unsafe-flabs f) 1.0)
              (set! ei (unsafe-fx+ ei 1))
              (set! f (unsafe-fl/ f 2.0))
              (loop)))
          (let loop ()
            (when (unsafe-fl< (abs f) 0.5)
              (set! ei (unsafe-fx- ei 1))
              (set! f (unsafe-fl* f 2.0))
              (loop)))
          (values
           f
           ei)))))

;;; -----------------------------------------------------------------
;;; Testing the Sign of Numbers

;;; (sign x) -> (or/c -1 1)
;;;   x : real?
;;; Return the sign of a real number, 1 for positive (or zero),
;;; -1 for negative.
(define (sign x)
  (if (>= x 0) 1 -1))

;;; -----------------------------------------------------------------
;;; Testing form Odd and Even Numbers
;;;
;;; Scheme provides even? and odd?

;;; -----------------------------------------------------------------
;;; Maximimum and Minimum Functions
;;;
;;; Scheme provides min and max

;;; -----------------------------------------------------------------
;;; Approximate Comparison of Floating Point Numbers

;;; (fcmp x1 x2) -> (or/c -1 0 1)
;;;   x1 : real?
;;;   x2 : real?
;;; Compare two real for difference less than the specified epsilon.
;;; Returns -1 if x1 < x2; 0 if x1 ~=  x2; 1 if x1 > x2.
(define (fcmp x1 x2 epsilon)
  (with-float (x1 x2 epsilon)
    (let ((difference (unsafe-fl- x1 x2))
          (max (if (unsafe-fl> (unsafe-flabs x1) (unsafe-flabs x2)) x1 x2)))
      (let*-values (((dummy exponent) (frexp max))
                    ((delta) (ldexp epsilon exponent)))
        (if (unsafe-fl> difference delta)
            1                             ; x1 > x2
            (if (unsafe-fl< difference (unsafe-fl- 0.0 delta))
                -1                        ; x1 < x2
                0))))))                   ; x1 ~= x2

;;; log10 and dB

;;; (log10 x) -> inexact-real?
;;;   x : real?
;;; Returns the log base 10 of x.
(define (log10 x)
  (with-float (x)
    (unsafe-fl* 1/ln10 (unsafe-fllog x))))

;;; (dB x) -> inexact-real?
;;;   x : real?
;;; Returns the value of x in decibels, 10*log10(x).
(define (dB x)
  (with-float (x)
    (unsafe-fl* 10.0 (unsafe-fl* 1/ln10 (unsafe-fllog x)))))

;;; Module Contracts

(provide
 e
 log2e
 log10e
 sqrt2
 sqrt1/2
 sqrt3
 pi
 2*pi
 4*pi
 pi/2
 pi/4
 sqrtpi
 2/sqrtpi
 1/pi
 2/pi
 ln10
 1/ln10
 ln2
 1/ln2
 lnpi
 euler)

(provide/contract
 (nan?
  (-> any/c boolean?))
 (infinite
  (-> any/c (or/c -1 #f 1)))
 (infinite?
  (-> any/c boolean?))
 (finite?
  (-> any/c boolean?))
 (log1p
  (-> real? number?))
 (expm1
  (-> real? inexact-real?))
 (hypot
  (-> real? real? inexact-real?))
 (acosh
  (-> real? inexact-real?))
 (asinh
  (-> real? inexact-real?))
 (atanh
  (-> real? inexact-real?))
 (ldexp
  (-> real? integer? inexact-real?))
 (frexp
  (-> real? (values inexact-real? integer?)))
 (sign
  (-> real? (or/c -1 1)))
 (fcmp
  (-> real? real? real? (or/c -1 0 1)))
 (log10
  (-> real? inexact-real?))
 (dB
  (-> real? inexact-real?)))
