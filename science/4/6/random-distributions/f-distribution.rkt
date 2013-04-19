#lang racket
;;; Science Collection
;;; random-distributions/f-distribution.rkt
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
;;; This module implements the f-distribution.  It is based on the
;;; Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 1.1.0    02/08/06  Added cdf.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../random-source.ss"
         "../special-functions/gamma.ss"
         "gamma.ss"
         "cdf-beta-inc.ss")

;;; random-f-distribution: random-source x real x real -> real
;;; random-f-distribution: real x real -> real
;;; This function returns a random variate from the F-distribution
;;; with degrees of freedom nu1 and nu2.
(define random-f-distribution
  (case-lambda
    ((r nu1 nu2)
     (let ((y1 (unchecked-random-gamma r (/ nu1 2.0) 2.0))
           (y2 (unchecked-random-gamma r (/ nu2 2.0) 2.0)))
       (/ (* y1 nu2) (* y2 nu1))))
    ((nu1 nu2)
     (random-f-distribution (current-random-source) nu1 nu2))))

;;; f-distribution-pdf: real x real x real -> real
;;; This function computes the probability density p(x) at x of an
;;; f-distribution with degrees of freedom nu1 and nu2.
(define (f-distribution-pdf x nu1 nu2)
  (if (< x 0.0)
      0.0
      (let ((lglg (+ (* (/ nu1 2.0) (log nu1))
                     (* (/ nu2 2.0) (log nu2))))
            (lg12 (unchecked-lngamma (/ (+ nu1 nu2) 2.0)))
            (lg1 (unchecked-lngamma (/ nu1 2.0)))
            (lg2 (unchecked-lngamma (/ nu2 2.0))))
        (* (exp (+ lglg lg12 (- lg1) (- lg2)))
           (expt x (- (/ nu1 2.0) 1.0))
           (expt (+ nu2 (* nu1 x)) (- (/ (- nu1) 2.0) (/ nu2 2.0)))))))

;;; f-distribution-cdf: real x real x real -> real
;;; This function computes the cummulative density d(x) at x of an
;;; f-distribution with degrees of freedom nu1 and nu2.
(define (f-distribution-cdf-P x nu1 nu2)
  (let ((r (/ nu2 nu1)))
    (if (< x r)
        (let ((u (/ x (+ r x))))
          (beta-inc-axpy 1.0 0.0 (/ nu1 2.0) (/ nu2 2.0) u))
        (let ((u (/ r (+ r x))))
          (beta-inc-axpy -1.0 1.0 (/ nu2 2.0) (/ nu1 2.0) u)))))

(define (f-distribution-cdf-Q x nu1 nu2)
  (let ((r (/ nu2 nu1)))
    (if (< x r)
        (let ((u (/ x (+ r x))))
          (beta-inc-axpy -1.0 0.0 (/ nu1 2.0) (/ nu2 2.0) u))
        (let ((u (/ r (+ r x))))
          (beta-inc-axpy 1.0 1.0 (/ nu2 2.0) (/ nu1 2.0) u)))))

(define f-distribution-cdf f-distribution-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-f-distribution unchecked-random-f-distribution)
             (f-distribution-pdf unchecked-f-distribution-pdf)
             (f-distribution-cdf-P unchecked-f-distribution-cdf-P)
             (f-distribution-cdf-Q unchecked-f-distribution-cdf-Q)
             (f-distribution-cdf unchecked-f-distribution-cdf)))

(provide/contract
 (random-f-distribution
  (case-> (-> random-source? real? real? (>=/c 0.0))
          (-> real? real? (>=/c 0.0))))
 (f-distribution-pdf
  (-> real? real? real? (>=/c 0.0)))
 (f-distribution-cdf-P
  (-> real? real? real? (real-in 0.0 1.0)))
 (f-distribution-cdf-Q
  (-> real? real? real? (real-in 0.0 1.0)))
 (f-distribution-cdf
  (-> real? real? real? (real-in 0.0 1.0))))
