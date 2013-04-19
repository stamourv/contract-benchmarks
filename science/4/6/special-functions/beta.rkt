#lang racket
;;; Science Collection
;;; special-functions/beta.rkt
;;; Copyright (c) 2006-2011 M. Douglas Williams
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
;;; This is the module for the beta special function.
;;;
;;; Version  Date      Description
;;; 1.0.0    02/08/06  Initial implementation. (MDW)
;;; 2.0.0    11/17/07  Added unchecked version of function and getting ready for
;;;                    PLT Scheme v4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

(require "../math.ss"
         "gamma.ss")

(define (lnbeta x y)
  (let* ((xymax (max x y))
         (xymin (min x y))
         (rat (/ xymin xymax)))
    (if (< rat 0.2)
        ;; min << max, so be careful with the subtraction
        (let* ((gsx (unchecked-gamma* x))
               (gsy (unchecked-gamma* y))
               (gsxy (unchecked-gamma* (+ x y)))
               (lnopr (log1p rat))
               (lnpre (log (* (/ (* gsx gsy) gsxy)
                              sqrt2 sqrtpi)))
               (t1 (* xymin (log rat)))
               (t2 (* 0.5 (log xymin)))
               (t3 (* (+ x y -0.5) lnopr))
               (lnpow (- t1 t2 t3)))
          (+ lnpre lnpow))
        (let* ((lgx (unchecked-lngamma x))
               (lgy (unchecked-lngamma y))
               (lgxy (unchecked-lngamma (+ x y))))
          (+ lgx lgy (- lgxy))))))

(define (beta x y)
  (if (and (< x 50.0)
           (< y 50.0))
      (let ((gx (unchecked-gamma x))
            (gy (unchecked-gamma y))
            (gxy (unchecked-gamma (+ x y))))
        (/ (* gx gy) gxy))
      (exp (lnbeta x y))))

;;; Module Contracts

(provide
 (rename-out (beta unchecked-beta)
             (lnbeta unchecked-lnbeta)))

(provide/contract
 (beta 
  (-> (>/c 0.0) (>/c 0.0) real?))
 (lnbeta
  (-> (>/c 0.0) (>/c 0.0) real?)))
