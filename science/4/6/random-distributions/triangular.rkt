#lang racket
;;; Science Collection
;;; random-distributions/triangular.rkt
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
;;; This code implements a triangular distribution.
;;;
;;; Version  Date      Description
;;; 0.9.0    08/06/04  This is the initial release of the triangular
;;;                    distribution routines. (Doug Williams)
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/05  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../random-source.rkt")

;;; random-triangular: random-source x real x real x real -> real
;;; random-triangular: real x real x real -> real
;;; This function returns a random variate from a triangular
;;; distribution with min, a, max, b, and mode, c.
(define random-triangular
  (case-lambda
    ((r a b c)
     (unless (<= a c b)
       (error 'random-triangular
              "expected ~a <= ~a <= ~a" a c b))
     (let ((u (unchecked-random-uniform r)))
       (if (<= u (/ (- c a) (- b a)))
           (+ a (sqrt (* u (- b a) (- c a))))
           (- b (sqrt (* (- 1.0 u) (- b a) (- b c)))))))
    ((a b c)
     (random-triangular (current-random-source) a b c))))

;;; triangular-pdf: real x real x real x real -> real
;;; This function computes the probability density p(x) at x of a 
;;; triangular distribution with min, a, max, b, and mode, c.
(define (triangular-pdf x a b c)
  (if (< x a)
      ;; x < a
      0.0
      (if (<= x c)
          ;; a <= x <= c
          (/ (* 2.0 (- x a)) (* (- b a) (- c a)))
          (if (<= x b)
              ;; x <= x <= b
              (/ (* 2.0 (- b x)) (* (- b a) (- b c)))
              ;; x > b
              0))))

;;; triangular-cdf: real x real x real x real -> real
;;; This function computes the cummulative density d(x) at a for a
;;; triangular distribution with min, a, max, b, and mode, c.
(define (triangular-cdf-P x a b c)
  (if (< x a)
      0.0
      (if (<= x c)
          (/ (* (- x a) (- x a))
             (* (- b a) (- c a)))
          (if (<= x b)
              (- 1.0 (/ (* (- b x) (- b x))
                        (* (- b a) (- b c))))
              1.0))))

(define (triangular-cdf-Q x a b c)
  (if (> x b)
      0.0
      (if (>= x c)
          (/ (* (- b x) (- b x))
             (* (- b a) (- b c)))
          (if (>= x a)
              (- 1.0 (/ (* (- x a) (- x a))
                        (* (- b a) (- c a))))
              1.0))))

(define triangular-cdf triangular-cdf-P)

;;; Module Contracts

(provide
 (rename-out (random-triangular unchecked-random-triangular)
             (triangular-pdf unchecked-triangular-pdf)
             (triangular-cdf-P unchecked-triangular-cdf-P)
             (triangular-cdf-Q unchecked-triangular-cdf-Q)
             (triangular-cdf unchecked-triangular-cdf)))

(provide/contract
; (random-triangular
;  (case-> (->r ((r random-source?)
;                (a real?)
;                (b (>/c a))
;                (c (and/c (>=/c a)
;                          (<=/c b))))
;               real?)
;          (->r ((a real?)
;                (b (>/c a))
;                (c (and/c (>=/c a)
;                          (<=/c b))))
;               real?)))
 (random-triangular
  (case-> (-> random-source? real? real? real? real?)
          (-> real? real? real? real?)))
 (triangular-pdf
  (->i ((x real?)
        (a real?)
        (b (a) (>/c a))
        (c (a b) (and/c (>=/c a)
                        (<=/c b))))
       (result () (>=/c 0.0))))
 (triangular-cdf-P
  (->i ((x real?)
        (a real?)
        (b (a) (>/c a))
        (c (a b) (and/c (>=/c a)
                        (<=/c b))))
       (result () (real-in 0.0 1.0))))
 (triangular-cdf-Q
  (->i ((x real?)
        (a real?)
        (b (a) (>/c a))
        (c (a b) (and/c (>=/c a)
                        (<=/c b))))
       (result () (real-in 0.0 1.0))))
 (triangular-cdf
  (->i ((x real?)
        (a real?)
        (b (a) (>/c a))
        (c (a b) (and/c (>=/c a)
                        (<=/c b))))
       (result () (real-in 0.0 1.0)))))
