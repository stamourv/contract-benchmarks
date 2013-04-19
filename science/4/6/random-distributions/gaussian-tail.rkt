#lang racket
;;; Science Collection
;;; random-distributions/gaussian-tail.rkt
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
;;; This module implements the Gaussian tail distribution.  It is based
;;; on the Random Number Distributions.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "../math.rkt"
         "../random-source.rkt"
         "../special-functions/error.rkt"
         "gaussian.rkt")

;;; Gaussian tail distribution

;;; random-gaussian-tail: random-source x real x real x real -> real
;;; random-gaussian-tail" real x real x real -> real
;;; Returns a gaussian random variable larger than a.  This
;;; implementation does one-sided upper-tailed deviates.
(define random-gaussian-tail
  (case-lambda
    ((r a mu sigma)
     (let ((s (/ (- a mu) sigma)))
       (if (<= s 1.0)
           ;; For small s, use a direct rejection method.  The limit
           ;; s <= 1 can be adjusted to optimize overall effeciency.
           (let ((x 0.0))
             (let loop ()
               (set! x (unchecked-random-unit-gaussian r))
               (when (< x s) 
                 (loop)))
             (+ mu (* x sigma)))
           ;; Use the "supertail" deviates from the last two steps
           ;; of Marsaglia's rectangle-wedge-tail method, as described
           ;; in Knuth, v2, 3rd ed, pp 123-128.  (See also exercise 11, 
           ;; p139, and the solution, p586.)
           (let ((u 0.0)
                 (v 0.0)
                 (x 0.0))
             (let loop ()
               (set! u (unchecked-random-uniform r))
               (set! v (unchecked-random-uniform r))
               ;; Note: v > 0.0
               (set! x (sqrt (- (* s s) (* 2.0 (log v)))))
               (when (> (* x u) s)
                 (loop)))
             (+ mu (* x sigma))))))
    ((a mu sigma)
     (random-gaussian-tail (current-random-source) a mu sigma))))

;;; random-unit-gaussian-tail: random-source x real -> real
;;; random-unit-gaussian-tail: real -> real
(define random-unit-gaussian-tail
  (case-lambda
    ((r a)
     (random-gaussian-tail r a 0.0 1.0))
    ((a)
     (random-unit-gaussian-tail (current-random-source) a))))

;;; gaussian-tail-pdf: real x real x real x real -> real
;;; This function computes the probability density p(x) at x from the
;;; upper tail of a Gaussian distribution with mean mu and standard
;;; deviation sigma.
(define (gaussian-tail-pdf x a mu sigma)
  (if (< x a)
      0
      (let ((N 0.0)
            (p 0.0)
            (u (/ (- x mu) sigma))
            (f (unchecked-erfc (/ (- a mu) (* (sqrt 2.0) sigma)))))
        (set! N (* 0.5 f))
        (set! p (* (/ 1.0 (* N (sqrt (* 2.0 pi)) sigma))
                   (exp (/ (* (- u) u) 2.0))))
        p)))

;;; unit-gaussian-tail-pdf: real x real -> real
(define (unit-gaussian-tail-pdf x a)
  (gaussian-tail-pdf x a 0.0 1.0))

;;; Module Contracts

(provide
 (rename-out (random-gaussian-tail unchecked-random-gaussian-tail)
             (random-unit-gaussian-tail unchecked-random-unit-gaussian-tail)
             (gaussian-tail-pdf unchecked-gaussian-tail-pdf)
             (unit-gaussian-tail-pdf unchecked-unit-gaussian-tail-pdf)))

(provide/contract
 (random-gaussian-tail
  (case-> (-> random-source? real? real? (>=/c 0.0) real?)
          (-> real? real? (>=/c 0.0) real?)))
 (random-unit-gaussian-tail
  (case-> (-> random-source? real? real?)
          (-> real? real?)))
 (gaussian-tail-pdf
  (-> real? real? real? (>=/c 0.0) (>=/c 0.0)))
 (unit-gaussian-tail-pdf
  (-> real? real? (>=/c 0.0))))
