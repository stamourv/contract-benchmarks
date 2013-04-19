#lang racket
;;; Science Collection
;;; random-distributions/gamma.rkt
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
;;; This module implements the gamma distribution.  It is based on the
;;; Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added 
;;;                    contracts for functions.  (Doug Williams)
;;; 1.1.0    02/09/06  Added cdf.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "../machine.ss"
         "../math.ss"
         "../random-source.ss"
         "../special-functions/gamma.ss")

;;; random-gamma: random-source x real x real -> real
;;; random-gamma: real x real -> real
;;; This function returns a random variate from the gamma 
;;; distribution with parameters a and b.
(define random-gamma
  (case-lambda
    ((r a b)
     (let ((na (inexact->exact (floor a))))
       (cond ((= a na)
              (* b (random-gamma-int r na)))
             ((= na 0.0)
              (* b (random-gamma-frac r a)))
             (else
              (* b (+ (random-gamma-int r na)
                      (random-gamma-frac r (- a na))))))))
    ((a b)
     (random-gamma (current-random-source) a b))))

;;; random-gamma-int: random-source x integer -> real
(define (random-gamma-int r na)
  (if (< na 12)
      ;; Note: for 12 iterations we are safe against underflow,
      ;; since the smallest positive random number is O(2^-32).
      ;; This means the smallest product is 2^(12*32) = 10^-116,
      ;; which is in the range of double precision.
      (let ((prod 1.0))
        (do ((i 0 (+ i 1)))
            ((= i na) (- (log prod)))
          (set! prod (* prod (unchecked-random-uniform r)))))
      (random-gamma-large r na)))

;;; random-gamma-large: random-source x real -> real
(define (random-gamma-large r a)
  (let ((sqa (sqrt (- (* 2.0 a) 1.0)))
        (x 0.0)
        (y 0.0)
        (v 0.0))
    (let loop1 ()
      (let loop2 ()
        (set! y (tan (* pi (unchecked-random-uniform r))))
        (set! x (+ (* sqa y) a -1))
        (when (<= x 0.0) (loop2)))
      (set! v (unchecked-random-uniform r))
      (when (> v (- (* (+ 1.0 (* y y)) 
                       (exp (- (* (- a 1.0) (log (/ x (- a 1.0))))
                               (* sqa y))))))
        (loop1)))
    x))

;;; random-gamma-frac: random-source x real -> real
(define (random-gamma-frac r a)
  (let ((p (/ e (+ a e)))
        (q 0.0)
        (x 0.0)
        (u 0.0)
        (v 0.0))
    (let loop ()
      (set! u (unchecked-random-uniform r))
      (set! v (unchecked-random-uniform r))
      (if (< u p)
          (begin
            (set! x (exp (* (/ 1.0 a) (log v))))
            (set! q (exp (- x))))
          (begin
            (set! x (- 1.0 (log v)))
            (set! q (exp (* (- a 1.0) (log x))))))
      (when (>= (unchecked-random-uniform r) q)
        (loop)))
    x))

;;; gamma-pdf: real x real x real -> real
;;; This function computes the probability density p(x) for a gamma 
;;; function with parameters a and b.
(define (gamma-pdf x a b)
  (cond ((< x 0)
         0.0)
        ((= x 0.0)
         (if (= a 1.0)
             (/ 1.0 b)
             0.0))
        ((= a 1.0)
         (exp (/ (/ (- x) b) b)))
        (else
         (/ (exp (+ (* (- a 1.0) (log (/ x b)))
                    (- (/ x b))
                    (- (unchecked-lngamma a))))
            b))))

;;; cdf
(define LARGE-A 85)

(define (norm-arg x a)
  (let ((t 0.0)
        (arg (+ (x (/ 1.0 3.0) (- a) (- (/ 0.02 a)))))
        (u (/ (- a 0.5) x)))
    (cond ((< (abs (- u 1.0)) double-epsilon)
           (set! t 0.0))
          ((< (abs u) double-epsilon)
           (set! t 1.0))
          ((> u 0.0)
           (let ((v (- 1.0 u)))
             (set! t (/ (+ 1.0 (- (* u u)) (* 2.0 u (log u))) (* v v)))))
          (else
           (set! t +nan.0)))
    (* arg (sqrt (/ (+ 1.0 t) x)))))

;;; Wrapper for functions that do the work.
(define (gamma-cdf-P x a b)
  (if (<= x 0.0)
      0.0
      (let ((y (/ x b)))
        (if (> y a)
            (- 1.0 (unchecked-gamma-inc-Q a y))
            (unchecked-gamma-inc-P a y)))))

(define (gamma-cdf-Q x a b)
  (if (<= x 0.0)
      1.0
      (let ((y (/ x b)))
        (if (< y a)
            (- 1.0 (unchecked-gamma-inc-P a y))
            (unchecked-gamma-inc-Q a y)))))

(define gamma-cdf gamma-cdf-P)

(provide
 (rename-out (random-gamma unchecked-random-gamma)
             (random-gamma-int unchecked-random-gamma-int)
             (gamma-pdf unchecked-gamma-pdf)
             (gamma-cdf-P unchecked-gamma-cdf-P)
             (gamma-cdf-Q unchecked-gamma-cdf-Q)
             (gamma-cdf unchecked-gamma-cdf)))

;;; Module Contracts

(provide/contract
 (random-gamma
  (case-> (-> random-source? (>/c 0.0) real? (>=/c 0.0))
          (-> (>/c 0.0) real? (>=/c 0.0))))
 (random-gamma-int
  (-> random-source? natural-number/c (>=/c 0.0)))
 (gamma-pdf
  (-> real? (>/c 0.0) real? (>=/c 0.0)))
 (gamma-cdf-P
  (-> real? (>/c 0.0) real? (real-in 0.0 1.0)))
 (gamma-cdf-Q
  (-> real? (>/c 0.0) real? (real-in 0.0 1.0)))
 (gamma-cdf
  (-> real? (>/c 0.0) real? (real-in 0.0 1.0))))
