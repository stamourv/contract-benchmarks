#lang racket
;;; Science Collection
;;; random-distributions/cdf-beta-inc.rkt
;;; Copyright (c) 2006-2008 M. Douglas Williams
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
;;; This module implements the incomplete beta function for cdfs. It is based on
;;; the Random Number Distributions in the GNU Scientific Library.
;;;
;;; Version  Date      Description
;;; 1.0.0    02/88/06  Initial version for Release 2.1. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

(require "../machine.rkt"
         "../math.rkt"
         "../special-functions/beta.rkt"
         "../special-functions/gamma.rkt")

(define (beta-cont-frac a b x epsabs)
  (define max-iter 512)
  (define cutoff (* 2.0 double-min))
  (let ((iter-count 0)
        (cf 0.0)
        (num-term 1.0)
        (den-term (- 1.0 (/ (* (+ a b) x) (+ a 1.0)))))
    (when (< (abs den-term) cutoff)
      (set! den-term +nan.0))
    (set! den-term (/ 1.0 den-term))
    (set! cf den-term)
    (let loop ()
      (when (< iter-count max-iter)
        (let* ((k (+ iter-count 1))
               (coeff (/ (* k (- b k) x)
                         (* (+ (- a 1.0) (* 2 k))
                            (+ a (* 2 k)))))
               (delta-frac 0.0))
          ;; first step
          (set! den-term (+ 1.0 (* coeff den-term)))
          (set! num-term (+ 1.0 (/ coeff num-term)))
          (when (< (abs den-term) cutoff)
            (set! den-term +nan.0))
          (when (< (abs num-term) cutoff)
            (set! num-term +nan.0))
          (set! den-term (/ 1.0 den-term))
          (set! delta-frac (* den-term num-term))
          (set! cf (* cf delta-frac))
          (set! coeff (/ (* (- (+ a k)) (+ a b k) x)
                         (* (+ a (* 2 k)) (+ a (* 2 k) 1.0))))
          ;; second step
          (set! den-term (+ 1.0 (* coeff den-term)))
          (set! num-term (+ 1.0 (/ coeff num-term)))
          (when (< (abs den-term) cutoff)
            (set! den-term +nan.0))
          (when (< (abs num-term) cutoff)
            (set! num-term +nan.0))
          (set! den-term (/ 1.0 den-term))
          (set! delta-frac (* den-term num-term))
          (set! cf  (* cf delta-frac))
          (when (and (>= (- delta-frac 1.0) (* 2.0 double-epsilon))
                     (>= (* cf (abs (- delta-frac 1.0))) epsabs))
            (set! iter-count (+ iter-count 1))
            (loop)))))
    (if (>= iter-count max-iter)
        +nan.0
        cf)))

;;; The function (beta-inc-axpy acap ycap a b x) computes
;;; acap * beta-inc(a b x) + ycap taking account of possible
;;; cancellations when using the hypergeometric transformation
;;; beta-inc(a b x) = 1 - beta-inc(b a (- 1 x)).
;;;
;;; It also adjusts the accuracy of beta-inc() to fit the overall
;;; absolute error when acap*beta-inc is added to ycap, (e.g. if
;;; ycap >> acap*beta-inc, then the accuracy of beta-inc can be
;;; reduced)
(define (beta-inc-axpy acap ycap a b x)
  (cond ((= x 0.0)
         (+ (* acap 0) ycap))
        ((= x 1.0)
         (+ (* acap 1) ycap))
        ((and (> a 1.0e5)
              (< b 10.0)
              (> x (/ a (+ a b))))
         (let ((N (+ a (/ (- b 1.0) 2.0))))
           (+ (* acap (gamma-inc-Q b (* (- N) (log x)))) ycap)))
        ((and (> b 1.0e5)
              (< a 10.0)
              (< x (/ a (+ a b))))
         (let ((N (+ b (/ (- a 1.0) 2.0))))
           (+ (* acap (gamma-inc-P a (* (- N) (log1p (- x))))) ycap)))
        (else
         (let* ((ln-beta (lnbeta a b))
                (ln-pre (+ (- ln-beta)
                           (* a (log x))
                           (* b (log1p (- x)))))
                (prefactor (exp ln-pre)))
           (if (< x (/ (+ a 1.0) (+ a b 2.0)))
               (let* ((epsabs (* (abs (/ ycap
                                         (/ (* acap prefactor)
                                            a)))
                                 double-epsilon))
                      (cf (beta-cont-frac a b x epsabs)))
                 (+ (* acap (/ (* prefactor cf) a)) ycap))
               (let* ((epsabs (* (abs (/ (+ acap ycap)
                                         (/ (* acap prefactor)
                                            b)))
                                 double-epsilon))
                      (cf (beta-cont-frac b a (- 1.0 x) epsabs))
                      (term (/ (* prefactor cf) b)))
                 (if (= acap (- ycap))
                     (* (- acap) term)
                     (+ (* acap (- 1.0 term))
                        ycap))))))))

;;; Module Contracts

(provide
 (all-defined-out))
