#lang racket
;;; Science Collection
;;; random-distributions/logarithmic.rkt
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
;;; This module implements the logarithmic distribution.  It is based
;;; on the Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "../random-source.rkt")

;;; random-logarithmic: random-source x real -> integer
;;; random-logarithmic: real -> integer
;;; This function returns a random variate from a logarithmic
;;; distribution with probability p.
(define random-logarithmic
  (case-lambda
    ((r p)
     (let ((c (log (- 1.0 p)))
           (v (unchecked-random-uniform r)))
       (if (>= v p)
           1
           (let* ((u (unchecked-random-uniform r))
                  (q (- 1.0 (exp (* c u)))))
             (cond ((<= v (* q q))
                    (inexact->exact
                     (truncate
                      (+ 1.0 (/ (log v) (log q))))))
                   ((<= v q)
                    2)
                   (else
                    1))))))
    ((p)
     (random-logarithmic (current-random-source) p))))

;;; logarithmic-pdf: integer x real -> real
;;; This function computes the probability density p(x) at x for a
;;; logarithmic distribution with probability p.
(define (logarithmic-pdf k p)
  (if (= k 0)
      0.0
      (/ (expt p (exact->inexact k))
         (exact->inexact k)
         (log (/ 1.0 (- 1.0 p))))))

;;; Module Contracts

(provide
 (rename-out (random-logarithmic unchecked-random-logarithmic)
             (logarithmic-pdf unchecked-logarithmic-pdf)))

(provide/contract
 (random-logarithmic
  (case-> (-> random-source? (real-in 0.0 1.0) integer?)
          (-> (real-in 0.0 1.0) integer?)))
 (logarithmic-pdf
  (-> integer? (real-in 0.0 1.0) (>=/c 0.0))))
