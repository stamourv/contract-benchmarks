#lang racket
;;; Science Collection
;;; random-distributions/t-distribution.rkt
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
;;; This module implements the t-distribution.  It is based on the 
;;; Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 1.0.1    02/08/06  Added cdf.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../math.rkt"
         "../random-source.rkt"
         "../special-functions/gamma.rkt"
         "cdf-beta-inc.rkt"
         "gaussian.rkt"
         "chi-squared.rkt"
         "exponential.rkt")

;;; random-t-distribution: random-source x real -> real
;;; random-t-distribution: real -> real
;;; This function returns a random-variate from the t-distribution
;;;; with nu degrees of freedom.
(define random-t-distribution
  (case-lambda
    ((r nu)
     (if (<= nu 2)
         (let ((y1 (unchecked-random-unit-gaussian r))
               (y2 (unchecked-random-chi-squared r nu)))
           (/ y1 (sqrt (/ y2 nu))))
         (let ((y1 0)
               (y2 0)
               (z 0))
           (let loop ()
             (set! y1 (unchecked-random-unit-gaussian r))
             (set! y2 (unchecked-random-exponential r (/ 1 (- (/ nu 2) 1))))
             (set! z (/ (* y1 y1) (- nu 2)))
             (when (or (< (- 1 z) 0)
                       (> (exp (- (- y2) z)) (- 1 z)))
               (loop)))
           (* (/ y2 (sqrt (- 1 (/ 2 nu)))) (- 1 z)))))
    ((nu)
     (random-t-distribution (current-random-source) nu))))

;;; t-distribution-pdf: real x real -> real
;;; This function computes the probability density p(x) at x for a
;;; t-distribution with nu degrees of freedom.
(define (t-distribution-pdf x nu)
  (let ((lg1 (unchecked-lngamma (/ nu 2.0)))
        (lg2 (unchecked-lngamma (/ (+ nu 1.0) 2.0))))
    (* (/ (exp (- lg2 lg1)) (sqrt (* pi nu)))
       (expt (+ 1.0 (/ (* x x) nu)) (/ (- (+ nu 1.0)) 2.0)))))

;;; cdf implementation

(define (poly-eval c n x)
  (let ((y (* (vector-ref c 0) x)))
    (do ((i 1 (+ i 1)))
        ((= i n)(+ y (vector-ref c n)))
      (set! y (* x (+ y (vector-ref c i)))))))

(define (cornish-fisher t n)
  (define coeffs6 '#(0.265974025974025974026
                     5.449696969696969696970
                     122.20294372294372294372
                     2354.7298701298701298701
                     37625.00902597402597403
                     486996.1392857142857143
                     4960870.65
                     37978595.55
                     201505390.875
                     622437908.625))
  (define coeffs5 '#(0.2742857142857142857142
                     4.499047619047619047619
                     78.45142857142857142857
                     1118.710714285714285714
                     12387.6
                     101024.55
                     559494.0
                     1764959.625))
  (define coeffs4 '#(0.3047619047619047619048
                     3.752380952380952380952
                     46.67142857142857142857
                     427.5
                     2587.5
                     8518.5))
  (define coeffs3 '#(0.4
                     3.3
                     24.0
                     85.5))
  (let* ((a (- n 0.5))
         (b (* 48.0 a a))
         (z2 (* a (log1p (/ (* t t) n))))
         (z (sqrt z2))
         (p5 (* z (poly-eval coeffs6 9 z2)))
         (p4 (* (- z) (poly-eval coeffs5 7 z2)))
         (p3 (* z (poly-eval coeffs4 5 z2)))
         (p2 (* (- z) (poly-eval coeffs3 3 z2)))
         (p1 (* z (+ z2 3.0)))
         (p0 z)
         (y p5))
    (set! y (+ (/ y b) p4))
    (set! y (+ (/ y b) p3))
    (set! y (+ (/ y b) p2))
    (set! y (+ (/ y b) p1))
    (set! y (+ (/ y b) p0))
    (if (< t 0.0)
        (* y -1.0)
        y)))

;;; t-distribution-cdf: real x real -> real
(define (t-distribution-cdf-P x nu)
  (let ((x2 (* x x)))
    (cond ((and (> nu 30.0)
                (< x2 (* 10.0 nu)))
           (let ((u (cornish-fisher x nu)))
             (unchecked-unit-gaussian-cdf-P  u)))
          ((< x2 nu)
           (let* ((u (/ x2 nu))
                  (eps (/ u (+ 1.0 u))))
             (if (>= x 0.0)
                 (beta-inc-axpy 0.5 0.5 0.5 (/ nu 2.0) eps)
                 (beta-inc-axpy -0.5 0.5 0.5 (/ nu 2.0) eps))))
          (else
           (let* ((v (/ nu (* x x)))
                  (eps (/ v (+ 1.0 v))))
             (if (>= x 0.0)
                 (beta-inc-axpy -0.5 1.0 (/ nu 2.0) 0.5 eps)
                 (beta-inc-axpy 0.5 0.0 (/ nu 2.0) 0.5 eps)))))))

(define (t-distribution-cdf-Q x nu)
  (let ((x2 (* x x)))
    (cond ((and (> nu 30.0)
                (< x2 (* 10.0 nu)))
           (let ((u (cornish-fisher x nu)))
             (unchecked-unit-gaussian-cdf-Q  u)))
          ((< x2 nu)
           (let* ((u (/ x2 nu))
                  (eps (/ u (+ 1.0 u))))
             (if (>= x 0.0)
                 (beta-inc-axpy -0.5 0.5 0.5 (/ nu 2.0) eps)
                 (beta-inc-axpy 0.5 0.5 0.5 (/ nu 2.0) eps))))
          (else
           (let* ((v (/ nu (* x x)))
                  (eps (/ v (+ 1.0 v))))
             (if (>= x 0.0)
                 (beta-inc-axpy 0.5 1.0 (/ nu 2.0) 0.5 eps)
                 (beta-inc-axpy -0.5 0.0 (/ nu 2.0) 0.5 eps)))))))

(define t-distribution-cdf t-distribution-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-t-distribution unchecked-random-t-distribution)
             (t-distribution-pdf unchecked-t-distribution-pdf)
             (t-distribution-cdf-P unchecked-t-distribution-cdf-P)
             (t-distribution-cdf-Q unchecked-t-distribution-cdf-Q)
             (t-distribution-cdf unchecked-t-distribution-cdf)))

(provide/contract
 (random-t-distribution
  (case-> (-> random-source? real? real?)
          (-> real? real?)))
 (t-distribution-pdf
  (-> real? real? (>=/c 0.0)))
 (t-distribution-cdf-P
  (-> real? real? (real-in 0.0 1.0)))
 (t-distribution-cdf-Q
  (-> real? real? (real-in 0.0 1.0)))
 (t-distribution-cdf
  (-> real? real? (real-in 0.0 1.0))))
