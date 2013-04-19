#lang racket
;;; Science Collection
;;; random-distributions/pareto.rkt
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
;;; This module implements the pareto distribution.  It is based on the
;;; Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../random-source.rkt")

;;; random-pareto: ransom-source x real x real -> real
;;; random-pareto: real x real -> real
;;; This function returns a random variate from the Pareto
;;; distribution of order a.  The distribution funtion is
;;;   p(x) dx = (a/b) / (x/b)^(a+1) dx
(define random-pareto
  (case-lambda
    ((r a b)
     (let* ((x (unchecked-random-uniform r))
            (z (expt x (/ -1.0 a))))
       (* b z)))
    ((a b)
     (random-pareto (current-random-source) a b))))

;;; pareto-pdf: real x real x real -> real
;;; This function computes the probability density function p(x) at x
;;; for a Pareto distribution with exponent a and scale b using the
;;; formula above.
(define (pareto-pdf x a b)
  (if (>= x b)
      (/ (/ a b) (expt (/ x b) (+ a 1.0)))
      0.0))

;;; pareto-cdf: real x real x real -> real
;;; This function computes the cumulative density function D(x) for
;;; the Pareto distribution with exponent a and scale b.
(define (pareto-cdf-P x a b)
  (if (< x b)
      0.0
      (- 1.0 (expt (/ b x) a))))

(define (pareto-cdf-Q x a b)
  (if (< x b)
      0.0
      (expt (/ b x) a)))

(define pareto-cdf pareto-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-pareto unchecked-random-pareto)
             (pareto-pdf unchecked-pareto-pdf)
             (pareto-cdf-P unchecked-pareto-cdf-P)
             (pareto-cdf-Q unchecked-pareto-cdf-Q)
             (pareto-cdf unchecked-pareto-cdf)))

(provide/contract
 (random-pareto
  (case-> (-> random-source? real? real? real?)
          (-> real? real? real?)))
 (pareto-pdf
  (-> real? real? real? (>=/c 0.0)))
 (pareto-cdf-P
  (-> real? real? real? (real-in 0.0 1.0)))
 (pareto-cdf-Q
  (-> real? real? real? (real-in 0.0 1.0)))
 (pareto-cdf
  (-> real? real? real? (real-in 0.0 1.0))))
