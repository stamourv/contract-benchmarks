#lang racket
;;; Science Collection
;;; random-distributions/flat.rkt
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
;;; This code implements a flat (uniform) distribution.  It is based on
;;; the Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 0.9.0    08/06/04  This is the initial release of the flat
;;;                    distribution routines. (Doug Williams)
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../random-source.rkt")

;;; random-flat: random-source x real x real -> real
;;; random-flat: real x real -> real
;;; This function returns a random variate from the flat (uniform)
;;; distribution from a to b.
(define random-flat
  (case-lambda
    ((r a b)
     (when (<= b a)
       (error 'random-flat
              "null range [~a, ~a]" a b))
     (let ((u (unchecked-random-uniform r)))
       (+ (* a (- 1 u)) (* b u))))
    ((a b)
     (random-flat (current-random-source) a b))))

;;; flat-pdf: real x real x real-> real
;;; This function computes the probability density p(x) at x for a
;;; flat (uniform) distribution from a to b.
(define (flat-pdf x a b)
  (if (< x a)
      0.0
      (if (<= x b)
          (/ 1.0 (- b a))
          0.0)))

;;; flat-cdf: real x real x real-> real
;;; This function computes the cummulative density d(x) at x for a
;;; flat (uniform) distribution from a to b.
(define (flat-cdf-P x a b)
  (if (< x a)
      0.0
      (if (<= x b)
          (/ (- x a) (- b a))
          1.0)))

(define (flat-cdf-Q x a b)
  (if (< x a)
      1.0
      (if (> x b)
          0.0
          (/ (- b x) (- b a)))))

(define flat-cdf flat-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-flat unchecked-random-flat)
             (flat-pdf unchecked-flat-pdf)
             (flat-cdf-P unchecked-flat-cdf-P)
             (flat-cdf-Q unchecked-flat-cdf-Q)
             (flat-cdf unchecked-flat-cdf)))

(provide/contract
; (random-flat
;  (case-> (->i ((r random-source?)
;                (a real?)
;                (b (a) (>/c a)))
;               (result () real?))
;          (->i ((a real?)
;                (b (a) (>/c a)))
;               (result () real?))))
 (random-flat
  (case-> (-> random-source? real? real? real?)
          (-> real? real? real?)))
 (flat-pdf
  (->i ((x real?)
        (a real?)
        (b (a) (>/c a)))
       (result () (>=/c 0.0))))
 (flat-cdf-P
  (->i ((x real?)
        (a real?)
        (b (a) (>=/c a)))
       (result () (real-in 0.0 1.0))))
 (flat-cdf-Q
  (->i ((x real?)
        (a real?)
        (b (a) (>=/c a)))
       (result () (real-in 0.0 1.0))))
 (flat-cdf
  (->i ((x real?)
        (a real?)
        (b (a) (>=/c a)))
       (result () (real-in 0.0 1.0)))))
