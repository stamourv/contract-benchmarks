#lang racket
;;; Science Collection
;;; special-functions/error.rkt
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
;;; This code in based on the Error Functions in the GNU Scientific
;;; Library (GSL).
;;;
;;; Version  Date      Description
;;; 0.1.0    08/05/04  This is the initial release of the guassion distribution
;;;                    routines ported from GSL. (MDW)
;;; 1.0.0    09/28/04  Added log-erfc and hazard functions. Marked as ready for
;;                     Release 1.0. Added contracts for functions. (MDW)
;;; 2.0.0    11/17/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme V4.0. (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0. (Doug Williams)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

(require "../machine.rkt"
         "../math.rkt"
         "../chebyshev.rkt"
         "../unsafe-ops-utils.rkt"
         racket/unsafe/ops)

;;; Estimates erfc(x) valid for 8 < x < 100.
;;; This is based on index 5725 in Hart, et. al.
(define (erfc8-sum x)
  (define P
    #(2.97886562639399288862
      7.409740605964741794425
      6.1602098531096305440906
      5.019049726784267463450058
      1.275366644729965952479585264
      0.5641895854377550741253201704))
  (define Q
    #(3.3690752069827527677
      9.608965327192787870698
      17.08144074746600431571095
      12.0489519278551290360340491
      9.396034016235054150430579648
      2.260528520767326969591866945
      1.0))
  (let ((num (unsafe-vector-ref P 5))
        (den (unsafe-vector-ref Q 6)))
;    (do ((i 4 (- i 1)))
;        ((< i 0) (void))
;      (set! num (+ (* x num) (vector-ref P i))))
    (for ((i (in-range 4 -1 -1)))
      (set! num (unsafe-fl+ (unsafe-fl* x num) (unsafe-vector-ref P i))))
;    (do ((i 5 (- i 1)))
;        ((< i 0) (void))
;      (set! den (+ (* x den) (vector-ref Q i))))
    (for ((i (in-range 5 -1 -1)))
      (set! den (unsafe-fl+ (unsafe-fl* x den) (unsafe-vector-ref Q i))))
    (unsafe-fl/ num den)))

(define (erfc8 x)
  (let ((e (erfc8-sum x)))
    (unsafe-fl* e (unsafe-flexp (unsafe-fl* (unsafe-fl- 0.0 x) x)))))

(define (log-erfc8 x)
  (let ((e (erfc8-sum x)))
    (unsafe-fl- (unsafe-fllog e) (unsafe-fl* x x))))

;;; Abramowitz+Stegun, 7.1.5
(define (erfseries x)
  (let* ((coef x)
         (e coef)
         (del 0.0))
;    (do ((k 1 (+ k 1)))
;        ((= k 30) (void))
;      (set! coef (* coef (/ (* (- x) x) k)))
;      (set! del (/ coef (+ (* 2.0 k) 1.0)))
;      (set! e (+ e del)))
    (for ((k (in-range 1 30)))
      (set! coef (unsafe-fl* coef (unsafe-fl/ (unsafe-fl* (unsafe-fl- 0.0 x) x) (unsafe-fx->fl k))))
      (set! del (unsafe-fl/ coef (unsafe-fl+ (unsafe-fl* 2.0 (unsafe-fx->fl k)) 1.0)))
      (set! e (unsafe-fl+ e del)))
    (unsafe-fl* (unsafe-fl/ 2.0 (unsafe-flsqrt pi)) e)))

;;; Chebyshev fit for erfc((t+1)/2, -1 < t < 1
(define erfc-xlt1-data
  #( 1.06073416421769980345174155056
    -0.42582445804381043569204735291
     0.04955262679620434040357683080
     0.00449293488768382749558001242
    -0.00129194104658496953494224761
    -0.00001836389292149396270416979
     0.00002211114704099526291538556
    -5.23337485234257134673693179020e-7      
    -2.78184788833537885382530989578e-7
     1.41158092748813114560316684249e-8
     2.72571296330561699984539141865e-9
    -2.06343904872070629406401492476e-10
    -2.14273991996785367924201401812e-11
     2.22990255539358204580285098119e-12
     1.36250074650698280575807934155e-13
    -1.95144010922293091898995913038e-14
    -6.85627169231704599442806370690e-16
     1.44506492869699938239521607493e-16
     2.45935306460536488037576200030e-18
    -9.29599561220523396007359328540e-19))

(define erfc-xlt1-cs
  (make-chebyshev-series
   erfc-xlt1-data
   19 -1.0 1.0))

;;; Chebyshev fit for erfc(x) exp(x^2), 1 < x < 5, x = 2t + 3,
;;; -1 < t < 1
(define erfc-x15-data
  #( 0.44045832024338111077637466616
    -0.143958836762168335790826895326
     0.044786499817939267247056666937
    -0.013343124200271211203618353102
     0.003824682739750469767692372556
    -0.001058699227195126547306482530
     0.000283859419210073742736310108
    -0.000073906170662206760483959432
     0.000018725312521489179015872934
    -4.62530981164919445131297264430e-6
     1.11558657244432857487884006422e-6
    -2.63098662650834130067808832725e-7
     6.07462122724551777372119408710e-8
    -1.37460865539865444777251011793e-8
     3.05157051905475145520096717210e-9
    -6.65174789720310713757307724790e-10
     1.42483346273207784489792999706e-10
    -3.00141127395323902092018744545e-11
     6.22171792645348091472914001250e-12
    -1.26994639225668496876152836555e-12
     2.55385883033257575402681845385e-13
    -5.06258237507038698392265499770e-14
     9.89705409478327321641264227110e-15
    -1.90685978789192181051961024995e-15
     3.50826648032737849245113757340e-16))

(define erfc-x15-cs
  (make-chebyshev-series
   erfc-x15-data
   24 -1.0 1.0))

;;; Chebyshev fit for erfc(x) x exp(x^2), 5 < x < 10,
;;; x = (5t + 15)/2, -1 < t < 1
(define erfc-x510-data
  #( 1.11684990123545698684297865808
     0.003736240359381998520654927536
    -0.000916623948045470238763619870
     0.000199094325044940833965078819
    -0.000040276384918650072591781859
     7.76515264697061049477127605790e-6
    -1.44464794206689070402099225301e-6
     2.61311930343463958393485241947e-7
    -4.61833026634844152345304095560e-8
     8.00253111512943601598732144340e-9
    -1.36291114862793031395712122089e-9
     2.28570483090160869607683087722e-10
    -3.78022521563251805044056974560e-11
     6.17253683874528285729910462130e-12
    -9.96019290955316888445830597430e-13
     1.58953143706980770269606726000e-13
    -2.51045971047162509999527428316e-14
     3.92607828989125810013581287560e-15
    -6.07970619384160374392535453420e-16
     9.12600607264794717315507477670e-17))

(define erfc-x510-cs
  (make-chebyshev-series
   erfc-x510-data
   19 -1.0 1.0))

;;; Complementary error function, erfc
;;; This function computes the complementary error function of x.
(define (erfc x)
  (with-float (x)
    (let* ((ax (abs x))
           (val 
            (cond ((unsafe-fl<= ax 1.0)
                   (let ((t (unsafe-fl- (unsafe-fl* 2.0 ax) 1.0)))
                     (unchecked-chebyshev-eval erfc-xlt1-cs t)))
                  ((unsafe-fl<= ax 5.0)
                   (let ((ex2 (unsafe-flexp (unsafe-fl* (unsafe-fl- 0.0 x) x)))
                         (t (unsafe-fl* 0.5 (unsafe-fl- ax 3.0))))
                     (unsafe-fl* ex2 
                                 (unchecked-chebyshev-eval erfc-x15-cs t))))
                  ((unsafe-fl<= ax 10.0)
                   (let ((exterm (unsafe-fl/ (unsafe-flexp (unsafe-fl* (unsafe-fl- 0.0 x) x)) ax))
                         (t (unsafe-fl/ (unsafe-fl- (unsafe-fl* 2.0 ax) 15.0) 5.0)))
                     (unsafe-fl* exterm 
                                 (unchecked-chebyshev-eval erfc-x510-cs t))))
                  (else
                   (erfc8 ax)))))
      (if (unsafe-fl< x 0.0)
          (unsafe-fl- 2.0 val)
          val))))

;;; log_erfc: real -> real
;;; This function computes the logarithm of the complementary error
;;; function.
(define (log-erfc x)
  (with-float (x)
    (cond ((unsafe-fl< (unsafe-fl* x x) root6-double-epsilon)
           (let* ((y (unsafe-fl/ x sqrtpi))
                  ;; series for -1/2 log(erfc(sqrt(pi) y))
                  (c3 (unsafe-fl/ (unsafe-fl- 4.0 pi) 3.0))
                  (c4 (unsafe-fl* 2.0 (unsafe-fl/ (unsafe-fl- 1.0 pi) 3.0)))
                  (c5 -0.001829764677455021)
                  (c6  0.02629651521057465)
                  (c7 -0.01621575378835404)
                  (c8  0.00125993961762116)
                  (c9  0.00556964649138)
                  (c10 -0.0045563339802)
                  (c11  0.0009461589032)
                  (c12  0.0013200243174)
                  (c13 -0.00142906)
                  (c14  0.00048204)
                  (series (unsafe-fl+
                           c8
                           (unsafe-fl*
                               y
                               (unsafe-fl+
                                c9
                                (unsafe-fl*
                                 y
                                 (unsafe-fl+
                                  c10
                                  (unsafe-fl*
                                   y
                                   (unsafe-fl+
                                    c11
                                    (unsafe-fl*
                                     y
                                     (unsafe-fl+
                                      c12
                                      (unsafe-fl*
                                       y 
                                       (unsafe-fl+
                                        c12
                                        (unsafe-fl* y c14))))))))))))))
             (set! series (unsafe-fl*
                           y
                           (unsafe-fl+
                            1.0
                            (unsafe-fl*
                             y
                             (unsafe-fl+
                              1.0
                              (unsafe-fl*
                               y
                               (unsafe-fl+
                                c3
                                (unsafe-fl*
                                 y
                                 (unsafe-fl+
                                  c4
                                  (unsafe-fl*
                                   y
                                   (unsafe-fl+
                                    c5
                                    (unsafe-fl*
                                     y
                                     (unsafe-fl+
                                      c6
                                      (unsafe-fl*
                                       y
                                       (unsafe-fl+
                                        c7
                                        (unsafe-fl* y series))))))))))))))))
             (unsafe-fl* -2.0 series)))
          ((unsafe-fl< (abs x) 1.0)
           (log1p (- (erf x))))
          ((unsafe-fl> x 8.0)
           (log-erfc8 x))
          (else
           (unsafe-fllog (erfc x))))))

;;; erf: real -> real
;;; This function computes the error function.
(define (erf x)
  (with-float (x)
    (if (unsafe-fl< (unsafe-flabs x) 1.0)
        (erfseries x)
        (unsafe-fl- 1.0 (erfc x)))))

;;; hazard: real -> real
;;; This function computes the hazard function for the normal
;;; distribution.
(define (hazard x)
  (with-float (x)
    (if (unsafe-fl< x 25.0)
        (let* ((ln-erfc-val (log-erfc (unsafe-fl/ x sqrt2)))
               (lnc -0.22579135264472743236)
               (arg (unsafe-fl-
                     (unsafe-fl-
                      lnc
                      (unsafe-fl* 0.5 (unsafe-fl* x x)))
                     ln-erfc-val)))
          (unsafe-flexp arg))
        (let* ((ix2 (unsafe-fl/ 1.0 (unsafe-fl* x x)))
               (corrB (unsafe-fl-
                       1.0
                       (unsafe-fl*
                        (unsafe-fl* 9.0 ix2)
                        (unsafe-fl- 1.0 (unsafe-fl* 11.0 ix2)))))
               (corrM (unsafe-fl- (unsafe-fl- 1.0 (unsafe-fl* 5.0 ix2))
                                  (unsafe-fl- 1.0 (unsafe-fl* (unsafe-fl* 7.0 ix2) corrB))))
               (corrT (unsafe-fl-
                       1.0
                       (unsafe-fl*
                        ix2
                        (unsafe-fl-
                         1.0
                         (unsafe-fl* (unsafe-fl* 3.0 ix2) corrM))))))
          (unsafe-fl/ x corrT)))))

;;; Module Contracts

(provide
 (rename-out (erfc unchecked-erfc)
             (log-erfc unchecked-log-erfc)
             (erf unchecked-erf)
             (hazard unchecked-hazard)))

(provide/contract
 (erfc
  (-> real? (real-in 0.0 2.0)))
 (log-erfc
  (-> real? real?))
 (erf
  (-> real? (real-in -1.0 1.0)))
 (hazard
  (-> real? (>=/c 0.0))))
