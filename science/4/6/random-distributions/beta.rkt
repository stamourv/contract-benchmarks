#lang racket
;;; Science Collection
;;; random-distributions/beta.rkt
;;; Copyright (c) 2004-2010 M. Douglas Williams
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
;;; This modules provides the implementation of the beta distribution. It is
;;; based on the Random Number Distributions in the GNU Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added contracts for
;;;                    functions. (MDW)
;;; 1.1.0    02/08/06  Added cdf. (MDW)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme 4.0 (MDW)
;;; 3.0.0    06/09/08  Changes requred for V4.0. (MDW)
;;; 4.0.0    06/11/10  Changed the header and restructured the code. (MDW)

(require "../random-source.rkt"
         "../special-functions/gamma.rkt"
         "gamma.rkt"
         "cdf-beta-inc.rkt")

;;; random-beta: random-source x real x real -> real
;;; random-beta: real x real -> real
;;; These functions return a random variate from the beta distribution
;;; with parameters a and b.
(define random-beta
  (case-lambda
    ((r a b)
     (let ((x1 (unchecked-random-gamma r a 1.0))
           (x2 (unchecked-random-gamma r b 1.0)))
       (/ x1 (+ x1 x2))))
    ((a b)
     (random-beta (current-random-source) a b))))

;;; beta-pdf: real x real x real -> real
;;; This function computes the probability density for the beta
;;; distribution with parameters a and b.
(define (beta-pdf x a b)
  (if (or (< x 0.0)
          (> x 1.0))
      0.0
      (let ((gab (unchecked-lngamma (+ a b)))
            (ga (unchecked-lngamma a))
            (gb (unchecked-lngamma b)))
        (* (exp (- gab ga gb))
           (expt x (- a 1.0))
           (expt (- 1.0 x) (- b 1.0))))))

;;; beta-cdf: real x real x real -> real
;;; This function computes the cummulative probability density for
;;; the beta distribution with parameters a and b.
(define (beta-cdf-P x a b)
  (cond ((< x 0.0)
         0.0)
        ((>= x 1.0)
         1.0)
        (else
         (beta-inc-axpy 1.0 0.0 a b x))))

(define (beta-cdf-Q x a b)
  (cond ((>= x 1.0)
         0.0)
        ((<= x 0.0)
         1.0)
        (else
         (beta-inc-axpy -1.0 1.0 a b x))))

(define beta-cdf beta-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-beta unchecked-random-beta)
             (beta-pdf unchecked-beta-pdf)
             (beta-cdf-P unchecked-beta-cdf-P)
             (beta-cdf-Q unchecked-beta-cdf-Q)
             (beta-cdf unchecked-beta-cdf)))

(provide/contract
 (random-beta
  (case-> (-> random-source? real? real?
              (real-in 0.0 1.0))
          (-> real? real?
              (real-in 0.0 1.0))))
 (beta-pdf
  (-> real? real? real? (>=/c 0.0)))
 (beta-cdf-P
  (-> real? real? real? (real-in 0.0 1.0)))
 (beta-cdf-Q
  (-> real? real? real? (real-in 0.0 1.0)))
 (beta-cdf
  (-> real? real? real? (real-in 0.0 1.0))))
