#lang racket
;;; Science Collection
;;; random-distributions/exponential.rkt
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
;;; This code in based on the Random Number Distributions in the GNU
;;; Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 0.9.0    08/07/04  This is the initial release of the exponential
;;;                    distribution routines ported from GSL. (Doug
;;;                    Williams)
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added 
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../random-source.rkt"
         "../math.rkt")

;;; random-exponential: random-source x real -> real
;;; random-exponential: real -> real
;; This function returns a random variate from the exponential
;; distribution with mean mu.
(define random-exponential
  (case-lambda
    ((r mu)
     (let ((u (unchecked-random-uniform r)))
       (* (- mu) (log u))))
    ((mu)
     (random-exponential (current-random-source) mu))))

;;; exponential-pdf: real x real -> real
;;; This function computes the probability  density p(x) at x for an
;;; exponential distribution with mean mu.
(define (exponential-pdf x mu)
  (if (< x 0.0)
      0.0
      (/ (exp (/ (- x) mu)) mu)))

;;; exponential-cdf: real x real -> real
;;; This function computer the cummulative density d(x) at x for an
;;; exponential distribution with mean mu.
(define (exponential-cdf-P x mu)
  (if (< x 0.0)
      0.0
      (- (expm1 (/ (- x) mu)))))

(define (exponential-cdf-Q x mu)
  (if (< x 0.0)
      1.0
      (exp (/ (- x) mu))))

(define exponential-cdf exponential-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-exponential unchecked-random-exponential)
             (exponential-pdf unchecked-exponential-pdf)
             (exponential-cdf unchecked-exponential-cdf-P)
             (exponential-cdf unchecked-exponential-cdf-Q)
             (exponential-cdf unchecked-exponential-cdf)))

(provide/contract
 (random-exponential
  (case-> (-> random-source? (>/c 0.0)
              (>=/c 0.0))
          (-> (>/c 0.0) (>=/c 0.0))))
 (exponential-pdf
  (-> real? (>/c 0.0) (>=/c 0.0)))
 (exponential-cdf-P
  (-> real? (>/c 0.0) (real-in 0.0 1.0)))
 (exponential-cdf-Q
  (-> real? (>/c 0.0) (real-in 0.0 1.0)))
 (exponential-cdf
  (-> real? (>/c 0.0) (real-in 0.0 1.0))))
