#lang racket
;;; Science Collection
;;; random-distributions/bivariate-gaussian.rkt
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
;;; This module implements the bivariate gaussian distribution. It is based on
;;; the Random Number Distributions in the GNU Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added contracts for
;;;                    functions. (MDW)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme 4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    06/11/10  Changed the header and restructured the code. (MDW)

(require "../math.rkt"
         "../random-source.rkt")

;;; random-bivariate-gaussian: random-source x real x real x real ->
;;;                              real x real
;;; random-bivariate-gaussian: real x real x real -> real x real
;;; This function generates a pair of correlated gaussian variates,
;;;; with mean zero, correlation coefficient rho, and standard
;;; deviations sigma-x and sigma-y in the x and y directions.  The
;;; bivariate gaussian distribution probability distribution is
;;;
;;;   p(x,y) dxdy = (1/(2 pi sigma_x sigma_y sqrt(r)))
;;;                   exp(- (x^2 + y^2 - 2 r x y)/(2c)) dxdy
;;;
;;; The correlation coefficient rho should lie between 1 and -1.
(define random-bivariate-gaussian
  (case-lambda
    ((r sigma-x sigma-y rho)
     (let ((u 0.0)
           (v 0.0)
           (r2 0.0)
           (scale 0.0))
       (let loop ()
         (set! u (+ -1.0 (* 2.0 (unchecked-random-uniform r))))
         (set! v (+ -1.0 (* 2.0 (unchecked-random-uniform r))))
         (set! r2 (+ (* u u) (* v v)))
         (when (or (> r2 1.0)
                   (= r2 0.0))
           (loop)))
       (set! scale (sqrt (/ (* -2.0 (log r2)) r2)))
       (values
        (* sigma-x u scale)
        (* sigma-y (+ (* rho u) (* (sqrt (- 1.0 (* rho rho))) v)) scale))))
    ((sigma-x sigma-y rho)
     (random-bivariate-gaussian (current-random-source) sigma-x sigma-y rho))))

;;; Bivariate-gaussian-pdf: real x real x real x real x real -> real
(define (bivariate-gaussian-pdf x y sigma-x sigma-y rho)
  (let ((u (/ x sigma-x))
        (v (/ y sigma-y))
        (c (- 1.0 (* rho rho))))
    (* (/ 1.0 (* 2.0 pi sigma-x sigma-y (sqrt c)))
       (exp (/ (- (+ (* u u) (* -2.0 rho u v) (* v v))) (* 2.0 c))))))

;;; Module Contracts

(provide
 (rename-out (random-bivariate-gaussian unchecked-random-bivariate-gaussian)
             (bivariate-gaussian-pdf unchecked-bivariate-gaussian-pdf)))

(provide/contract
 (random-bivariate-gaussian
  (case-> (-> random-source? (>=/c 0.0) (>=/c 0.0) (real-in -1.0 1.0)
              (values real? real?))
          (-> (>=/c 0.0) (>=/c 0.0) (real-in -1.0 1.0)
              (values real? real?))))
 (bivariate-gaussian-pdf
  (-> real? real? (>=/c 0.0) (>=/c 0.0) (real-in -1.0 1.0) real?)))
