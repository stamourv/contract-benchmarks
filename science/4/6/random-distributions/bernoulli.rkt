#lang racket
;;; Science Collection
;;; random-distributions/bernoulli.rkt
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
;;; -----------------------------------------------------------------------------
;;;
;;; This module implements the bernoulli distribution.  It is based on the Random
;;; Number Distribution in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0. Added contracts for
;;;                    functions. (MDW)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme 4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    06/08/10  Changed the header and restructured the code. (MDW)

(require "../random-source.rkt")

;;; random-bernoulli: random-source x real -> (and/c exact-integer? (integer-in 0 1))
;;; random-bernoulli: real -> integer
;; The bernoulli distribution has the form:
;; prob(0) = 1-p, prob(1) = p
(define random-bernoulli
  (case-lambda
    ((r p)
     (let ((u (unchecked-random-uniform r)))
       (if (< u p) 1 0)))
    ((p)
     (random-bernoulli (current-random-source) p))))

;;; bernoulli-pdf: integer x real -> real
;;; This function computes the probability density p(x) at x for a
;;; Bernoulli distribution with probability p.
(define (bernoulli-pdf k p)
  (cond ((= k 0) (- 1.0 p))
        ((= k 1) p)
        (else 0.0)))

;;; bernoulli-cdf: integer x real -> real
;; This function computes the cummulative density d(x) at x for a
;;; Bernoulli distribution with probability p.
(define (bernoulli-cdf k p)
  (cond ((= k 0) (- 1.0 p))
        ((= k 1) 1.0)
        (else 0.0)))

;;; Module Contracts

(provide
 (rename-out (random-bernoulli unchecked-random-bernoulli)
             (bernoulli-pdf unchecked-bernoulli-pdf)
             (bernoulli-cdf unchecked-bernoulli-cdf)))

(provide/contract
 (random-bernoulli
  (case-> (-> random-source? (real-in 0.0 1.0) (and/c exact-integer? (integer-in 0 1)))
          (-> (real-in 0.0 1.0) (and/c exact-integer? (integer-in 0 1)))))
 (bernoulli-pdf
  (-> exact-integer? (real-in 0.0 1.0) (real-in 0.0 1.0)))
 (bernoulli-cdf
  (-> exact-integer? (real-in 0.0 1.0) (real-in 0.0 1.0))))
