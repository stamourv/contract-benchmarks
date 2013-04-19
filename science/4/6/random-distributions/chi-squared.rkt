#lang racket
;;; Science Collection
;;; random-distributions/chi-squared.rkt
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
;;; This module implements the chi-squared distribution.  It is based
;;; on the Ranfom Number Distributions in the GNU Scientific
;;; Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 1.1.0    02/08/06  Added cdf.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../random-source.ss")
(require "../special-functions/gamma.ss")
(require "gamma.ss")

;;; random-chi-squared: random-source x real -> real
;;; This function returns a random variate from the chi-squared
;;; distribution with nu degrees of freedom.
(define random-chi-squared
  (case-lambda
    ((r nu)
     (* 2.0 (unchecked-random-gamma r (/ nu 2.0) 1.0)))
    ((nu)
     (random-chi-squared (current-random-source) nu))))

;;; chi-squared-pdf: real x real -> real
;;; This function computes the probability density p(x) at x for the
;;; chi-squared distribution with nu degrees of freedom.
(define (chi-squared-pdf x nu)
  (if (<= x 0.0)
      0.0
      (let ((lngamma-val (unchecked-lngamma (/ nu 2.0))))
        (/ (exp (- (* (- (/ nu 2.0) 1.0)
                      (log (/ x 2.0)))
                   (/ x 2.0) lngamma-val))
           2.0))))

;;; chi-squared-cdf: real x real -> real
;;; This function computes the cummulative density d(x) at x for the
;;; chi-squared distribution with nu degrees of freedom.
(define (chi-squared-cdf-P x nu)
  (unchecked-gamma-cdf-P x (/ nu 2.0) 2.0))

(define (chi-squared-cdf-Q x nu)
  (unchecked-gamma-cdf-Q x (/ nu 2.0) 2.0))

(define chi-squared-cdf chi-squared-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-chi-squared unchecked-random-chi-squared)
             (chi-squared-pdf unchecked-chi-squared-pdf)
             (chi-squared-cdf-P unchecked-chi-squared-cdf-P)
             (chi-squared-cdf-Q unchecked-chi-squared-cdf-Q)
             (chi-squared-cdf unchecked-chi-squared-cdf)))

(provide/contract
 (random-chi-squared
  (case-> (-> random-source? real? (>=/c 0.0))
          (-> real? (>=/c 0.0))))
 (chi-squared-pdf
  (-> real? real? (>=/c 0.0)))
 (chi-squared-cdf-P
  (-> real? real? (real-in 0.0 1.0)))
 (chi-squared-cdf-Q
  (-> real? real? (real-in 0.0 1.0)))
 (chi-squared-cdf
  (-> real? real? (real-in 0.0 1.0))))
