#lang racket
;;; Science Collection
;;; random-distributions/lognormal.rkt
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
;;; Yhis module implements the lognormal distribution.  It is based on
;;; the Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/23/04  Marked as ready for Release 1.0  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../math.rkt"
         "../random-source.rkt"
         "gaussian.rkt")

;;; random-lognormal: random-source x real x real -> real
;;; random-lognormal: real x real -> real
;;; This function returns a random variate from the lognormal
;;; distribution with parameters mu and sigma.
(define random-lognormal
  (case-lambda
    ((r mu sigma)
     (let ((u 0.0)
           (v 0.0)
           (r2 0.0)
           (normal 0.0))
       (let loop ()
         (set! u (+ -1.0 (* 2.0 (unchecked-random-uniform r))))
         (set! v (+ -1.0 (* 2.0 (unchecked-random-uniform r))))
         (set! r2 (+ (* u u) (* v v)))
         (when (> r2 1.0)
           (loop)))
       (set! normal (* u (sqrt (/ (* -2.0 (log r2)) r2))))
       (exp (+ (* sigma normal) mu))))
    ((mu sigma)
     (random-lognormal (current-random-source) mu sigma))))

;;; lognormal-pdf: real x real x real -> real
;;; This function computes the probability density p(x) at x for a
;;; lognormal distribution with parameters mu and sigma.
(define (lognormal-pdf x mu sigma)
  (if (<= x 0.0)
      0.0
      (let* ((u (/ (- (log x) mu) sigma))
             (p (* (/ 1.0 (* x (abs sigma) (sqrt (* 2.0 pi))))
                   (exp (/ (- (* u u)) 2.0)))))
        p)))

;;; lognormal-cdf: real x real x real -> real
;;; This function computes the cummulative density d(x) at x for a
;;; lognormal distribution with parameters mu and sigma.
(define (lognormal-cdf-P x mu sigma)
  (if (<= x 0.0)
      0.0
      (unchecked-unit-gaussian-cdf-P (/ (- (log x) mu) sigma))))

(define (lognormal-cdf-Q x mu sigma)
  (if (<= x 0.0)
      1.0
      (unchecked-unit-gaussian-cdf-Q (/ (- (log x) mu) sigma))))

(define lognormal-cdf lognormal-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-lognormal unchecked-random-lognormal)
             (lognormal-pdf unchecked-lognormal-pdf)
             (lognormal-cdf-P unchecked-lognormal-cdf-P)
             (lognormal-cdf-Q unchecked-lognormal-cdf-Q)
             (lognormal-cdf unchecked-lognormal-cdf)))

(provide/contract
 (random-lognormal
  (case-> (-> random-source? real? (>=/c 0.0) real?)
          (-> real? (>=/c 0.0) real?)))
 (lognormal-pdf
  (-> real? real? (>=/c 0.0) (>=/c 0.0)))
 (lognormal-cdf-P
  (-> real? real? (>=/c 0.0) (real-in 0.0 1.0)))
 (lognormal-cdf-Q
  (-> real? real? (>=/c 0.0) (real-in 0.0 1.0)))
 (lognormal-cdf
  (-> real? real? (>=/c 0.0) (real-in 0.0 1.0))))
