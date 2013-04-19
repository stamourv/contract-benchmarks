#lang racket
;;; Science Collection
;;; random-distributions/binomial.rlt
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
;;; This module implements the bernoulli distribution.  It is based on the
;;; Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0. Added contracts for
;;;                    functions. (MDW)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme 4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    06/11/10  Changed the header and restructured the code. (MDW)

(require "../random-source.rkt"
         "../special-functions/gamma.rkt"
         "beta.rkt")

;;; random-binomial: random-source x real x integer -> integer
;;; random-binomial: real x integer -> integer
;;; This routine returns a random variate from a binomial distribution
;;; with n independent trials and probability p.
(define random-binomial
  (case-lambda
    ((r p n)
     (let ((a 0)
           (b 0)
           (k 0))
       (let loop ()
         (when (> n 10)
           (begin
             (set! a (+ 1 (quotient n 2)))
             (set! b (+ 1 n (- a)))
             (let ((x (unchecked-random-beta 
                       r
                       (exact->inexact a)
                       (exact->inexact b))))
               (if (>= x p)
                   (begin
                     (set! n (- a 1))
                     (set! p (/ p x)))
                   (begin
                     (set! k (+ k a))
                     (set! n (- b 1))
                     (set! p (/ (- p x) (- 1.0 x))))))
             (loop))))
       (do ((i 0 (+ i 1)))
           ((= i n) k)
         (let ((u (unchecked-random-uniform r)))
           (when (< u p)
             (set! k (+ k 1)))))))
    ((p n)
     (random-binomial (current-random-source) p n))))

;;; binomial-pdf: integer x real x integer -> real
;;; This function computes the probability density p(x) at x for a
;;; binomial distribution with n independent trials and probability p.
(define (binomial-pdf k p n)
  (if (> k n)
      0.0
      (let ((ln_c_nk (unchecked-lnchoose n k)))
        (exp (+ ln_c_nk
                (* k (log p))
                (* (- n k)
                   (log (- 1.0 p))))))))

(define (binomial-cdf-P k p n)
  (if (>= k n)
      1.0
      (beta-cdf-Q p (+ k 1.0) (- n k))))

(define (binomial-cdf-Q k p n)
  (if (>= k n)
      0.0
      (beta-cdf-P p (+ k 1.0) (- n k))))

(define binomial-cdf binomial-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-binomial unchecked-random-binomial)
             (binomial-pdf unchecked-binomial-pdf)
             (binomial-cdf-P unchecked-binomial-cdf-P)
             (binomial-cdf-Q unchecked-binomial-cdf-Q)
             (binomial-cdf unchecked-binomial-cdf)))

(provide/contract
 (random-binomial
  (case-> (-> random-source? (real-in 0.0 1.0) exact-integer? exact-integer?)
          (-> (real-in 0.0 1.0) exact-integer? exact-integer?)))
 (binomial-pdf
  (-> exact-integer? (real-in 0.0 1.0) exact-integer? (real-in 0.0 1.0)))
 (binomial-cdf-P
  (-> exact-integer? (real-in 0.0 1.0) exact-integer? (real-in 0.0 1.0)))
 (binomial-cdf-Q
  (-> exact-integer? (real-in 0.0 1.0) exact-integer? (real-in 0.0 1.0)))
 (binomial-cdf
  (-> exact-integer? (real-in 0.0 1.0) exact-integer? (real-in 0.0 1.0)))
 )
