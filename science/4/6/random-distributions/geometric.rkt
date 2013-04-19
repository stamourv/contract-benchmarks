#lang racket
;;; Science Collection
;;; random-distributions.geometric.rkt
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
;;; This modules implements geometric distributions.  It is based on
;;; the Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "../random-source.rkt"
         "../math.rkt")

;;; random-geometric: random-source x real -> integer
;;; random-geometric: real -> integer
;;; This functions returns a random variate from a geometric
;;; distribution with probability p.
(define random-geometric
  (case-lambda
    ((r p)
     (let ((u (unchecked-random-uniform r)))
       (if (= p 1.0)
           1
           (+ (inexact->exact (truncate (/ (log u)
                                           (log (- 1.0 p)))))
              1))))
    ((p)
     (random-geometric (current-random-source) p))))

;;; geometric-pdf: integer x real -> real
;;; This function computes the probability density p(x) at x for a
;;; geometric distribution with probability p.
(define (geometric-pdf k p)
  (cond ((= k 0)
         0.0)
        ((= k 1)
         p)
        (else
         (* p (expt (- 1.0 p) (- k 1.0))))))

(define (geometric-cdf-P k p)
  (if (< k 1)
      0.0
      (if (< p 0.5)
          (- (expm1 (* k (log1p (- p)))))
          (- 1.0 (expt (- 1.0 p) k)))))

(define (geometric-cdf-Q k p)
  (if (< k 1)
      1.0
      (if (< p 0.5)
          (exp (* k (log1p (- p))))
          (expt (- 1.0 p) k))))

(define geometric-cdf geometric-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-geometric unchecked-random-geometric)
             (geometric-pdf unchecked-geometric-pdf)
             (geometric-cdf-P unchecked-geometric-cdf-P)
             (geometric-cdf-Q unchecked-geometric-cdf-Q)
             (geometric-cdf unchecked-geometric-cdf)))

(provide/contract
 (random-geometric
  (case-> (-> random-source? (real-in 0.0 1.0) integer?)
          (-> (real-in 0.0 1.0) integer?)))
 (geometric-pdf
  (-> integer? (real-in 0.0 1.0) (>=/c 0.0)))
 (geometric-cdf-P
  (-> integer? (real-in 0.0 1.0) (real-in 0.0 1.0)))
 (geometric-cdf-Q
  (-> integer? (real-in 0.0 1.0) (real-in 0.0 1.0)))
 (geometric-cdf
  (-> integer? (real-in 0.0 1.0) (real-in 0.0 1.0))))
