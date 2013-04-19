#lang racket
;;; Science Collection
;;; random-distributions/poisson.rkt
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
;;; This module implements the poisson distribution.  It is based on
;;; the Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added 
;;;                    contracts for functions (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)


(require "../random-source.rkt"
         "../special-functions/gamma.rkt"
         "gamma.rkt"
         "binomial.rkt")

;;; random-poisson: random-source x real -> integer
;;; random-poisson: real -> integer
;;; This function returns a random variate from a poisson distribution
;;; with mean mu.
(define random-poisson
  (case-lambda
    ((r mu)
     (let/ec exit
       (let ((k 0))
         (let loop ()
           (when (> mu 10.0)
             (let* ((m (inexact->exact
                        (truncate (* mu (/ 7.0 8.0)))))
                    (x (unchecked-random-gamma-int r m)))
               (if (>= x mu)
                   (exit (+ k (unchecked-random-binomial r (/ mu x) (- m 1))))
                   (begin
                     (set! k (+ k m))
                     (set! mu (- mu x))
                     (loop))))))
         ;; This following method works well when mu is small
         (let ((emu (exp (- mu)))
               (prod 1.0))
           (let loop ()
             (set! prod (* prod (unchecked-random-uniform r)))
             (set! k (+ k 1))
             (when (> prod emu)
               (loop)))
           (- k 1)))))
    ((mu)
     (random-poisson (current-random-source) mu))))

;;; poisson-pdf: integer -> real
;;; This function computes the probability density p(x) at x for a
;;; poisson distribution with mean mu.
(define (poisson-pdf k mu)
  (let ((lf (unchecked-lnfact k)))
    (exp (- (* (log mu) k) lf mu))))

(define (poisson-cdf-P k mu)
  (gamma-cdf-Q mu (+ k 1.0) 1.0))

(define (poisson-cdf-Q k mu)
  (gamma-cdf-P mu (+ k 1.0) 1.0))

(define poisson-cdf poisson-cdf-P)

;;; Module Contracts
(provide
 (rename-out (random-poisson unchecked-random-poisson)
             (poisson-pdf unchecked-poisson-pdf)
             (poisson-cdf-P unchecked-poisson-cdf-P)
             (poisson-cdf-Q unchecked-poisson-cdf-Q)
             (poisson-cdf unchecked-poisson-cdf)))

(provide/contract
 (random-poisson
  (case-> (-> random-source? (>/c 0.0) natural-number/c)
          (-> (>/c 0.0) natural-number/c)))
 (poisson-pdf
  (-> natural-number/c (>/c 0.0) (real-in 0.0 1.0)))
 (poisson-cdf-P
  (-> natural-number/c (>/c 0.0) (real-in 0.0 1.0)))
 (poisson-cdf-Q
  (-> natural-number/c (>/c 0.0) (real-in 0.0 1.0)))
 (poisson-cdf
  (-> natural-number/c (>/c 0.0) (real-in 0.0 1.0))))
