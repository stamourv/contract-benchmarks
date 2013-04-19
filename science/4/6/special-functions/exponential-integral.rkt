#lang racket
;;; Science Collection
;;; special-functions/exponential-integral.rkt
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
;;; This file implements the gamma functions.  It is based on the 
;;; Special Functions in the GNU Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 1.0.0    02/09/06  Initial implementation. (MDW)
;;; 2.0.0    11/17/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme V4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

(require "../machine.ss"
         "../math.ss"
         "../chebyshev.ss"
         "../unsafe-ops-utils.rkt"
         racket/unsafe/ops)

;;; Chebyshev series data
(define AE11-data
  '#( 0.121503239716065790
     -0.065088778513550150
      0.004897651357459670
     -0.000649237843027216
      0.000093840434587471
      0.000000420236380882
     -0.000008113374735904
      0.000002804247688663
      0.000000056487164441
     -0.000000344809174450
      0.000000058209273578
      0.000000038711426349
     -0.000000012453235014
     -0.000000005118504888
      0.000000002148771527
      0.000000000868459898
     -0.000000000343650105
     -0.000000000179796603
      0.000000000047442060
      0.000000000040423282
     -0.000000000003543928
     -0.000000000008853444
     -0.000000000000960151
      0.000000000001692921
      0.000000000000607990
     -0.000000000000224338
     -0.000000000000200327
     -0.000000000000006246
      0.000000000000045571
      0.000000000000016383
     -0.000000000000005561
     -0.000000000000006074
     -0.000000000000000862
      0.000000000000001223
      0.000000000000000716
     -0.000000000000000024
     -0.000000000000000201
     -0.000000000000000082
      0.000000000000000017))

(define AE11-cs
  (make-chebyshev-series
   AE11-data
   38 -1.0 1.0))

(define AE12-data
  '#( 0.582417495134726740
     -0.158348850905782750
     -0.006764275590323141
      0.005125843950185725
      0.000435232492169391
     -0.000143613366305483
     -0.000041801320556301
     -0.000002713395758640
      0.000001151381913647
      0.000000420650022012
      0.000000066581901391
      0.000000000662143777
     -0.000000002844104870
     -0.000000000940724197
     -0.000000000177476602
     -0.000000000015830222
      0.000000000002905732
      0.000000000001769356
      0.000000000000492735
      0.000000000000093709
      0.000000000000010707
     -0.000000000000000537
     -0.000000000000000716
     -0.000000000000000244
     -0.000000000000000058))

(define AE12-cs
  (make-chebyshev-series
   AE12-data
   24 -1.0 1.0))

(define E11-data
  '#(-16.11346165557149402600
       7.79407277874268027690
      -1.95540581886314195070
       0.37337293866277945612
      -0.05692503191092901938
       0.00721107776966009185
      -0.00078104901449841593
       0.00007388093356262168
      -0.00000620286187580820
       0.00000046816002303176
      -0.00000003209288853329
       0.00000000201519974874
      -0.00000000011673686816
       0.00000000000627627066
      -0.00000000000031481541
       0.00000000000001479904
      -0.00000000000000065457
       0.00000000000000002733
      -0.00000000000000000108))

(define E11-cs
  (make-chebyshev-series
   E11-data
   18 -1.0 1.0))

(define E12-data
  '#(-0.03739021479220279500
      0.04272398606220957700
     -0.13031820798497005440
      0.01441912402469889073
     -0.00134617078051068022
      0.00010731029253063780
     -0.00000742999951611943
      0.00000045377325690753
     -0.00000002476417211390
      0.00000000122076581374
     -0.00000000005485141480
      0.00000000000226362142
     -0.00000000000008635897
      0.00000000000000306291
     -0.00000000000000010148
      0.00000000000000000315))

(define E12-cs
  (make-chebyshev-series
   E12-data
   15 -1.0 1.0))

(define AE13-data
  '#(-0.605773246640603460
     -0.112535243483660900
      0.013432266247902779
     -0.001926845187381145
      0.000309118337720603
     -0.000053564132129618
      0.000009827812880247
     -0.000001885368984916
      0.000000374943193568
     -0.000000076823455870
      0.000000016143270567
     -0.000000003466802211
      0.000000000758754209
     -0.000000000168864333
      0.000000000038145706
     -0.000000000008733026
      0.000000000002023672
     -0.000000000000474132
      0.000000000000112211
     -0.000000000000026804
      0.000000000000006457
     -0.000000000000001568
      0.000000000000000383
     -0.000000000000000094
      0.000000000000000023))

(define AE13-cs
  (make-chebyshev-series
   AE13-data
   24 -1.0 1.0))

(define AE14-data
  '#(-0.18929180007530170
     -0.08648117855259871
      0.00722410154374659
     -0.00080975594575573
      0.00010999134432661
     -0.00001717332998937
      0.00000298562751447
     -0.00000056596491457
      0.00000011526808397
     -0.00000002495030440
      0.00000000569232420
     -0.00000000135995766
      0.00000000033846628
     -0.00000000008737853
      0.00000000002331588
     -0.00000000000641148
      0.00000000000181224
     -0.00000000000052538
      0.00000000000015592
     -0.00000000000004729
      0.00000000000001463
     -0.00000000000000461
      0.00000000000000148
     -0.00000000000000048
      0.00000000000000016
     -0.00000000000000005))

(define AE14-cs
  (make-chebyshev-series
   AE14-data
   25 -1.0 1.0))

(define xmaxt (- log-double-min))
(define xmax (- xmaxt (log xmaxt)))

;;; Implementation for E1, allowing for scaling by exp(x)
(define (expint-E1-impl x scale)
  (cond ((and (unsafe-fl< x (unsafe-fl- 0.0 xmax))
              (not scale))
         +inf.0)
        ((unsafe-fl<= x -10.0)
         (let ((s (unsafe-fl*
                   (unsafe-fl/ 1.0 x) 
                   (if scale 1.0 (unsafe-flexp (unsafe-fl- 0.0 x)))))
               (r (unchecked-chebyshev-eval
                   AE11-cs (unsafe-fl+ (unsafe-fl/ 20.0 x) 1.0))))
           (unsafe-fl* s (unsafe-fl+ 1.0 r))))
        ((unsafe-fl<= x -4.0)
         (let ((s (unsafe-fl* 
                   (unsafe-fl/ 1.0 x)
                   (if scale 1.0 (unsafe-flexp (unsafe-fl- 0.0 x)))))
               (r (unchecked-chebyshev-eval AE12-cs (/ (+ (/ 40.0 x) 7.0) 3.0))))
           (unsafe-fl* s (unsafe-fl+ 1.0 r))))
        ((unsafe-fl<= x -1.0)
         (let ((ln-term (unsafe-fl- 0.0 (unsafe-fllog (unsafe-flabs x))))
               (sf (if scale (unsafe-flexp x) 1.0))
               (r (unchecked-chebyshev-eval
                   E11-cs (unsafe-fl/ (unsafe-fl+ (unsafe-fl* 2.0 x) 5.0) 3.0))))
           (unsafe-fl* sf (unsafe-fl+ ln-term r))))
        ((unsafe-fl= x 0.0)
         -inf.0)
        ((unsafe-fl<= x 1.0)
         (let ((ln-term (unsafe-fl- 0.0 (unsafe-fllog (unsafe-flabs x))))
               (sf (if scale (unsafe-flexp x) 1.0))
               (r (unchecked-chebyshev-eval E12-cs x)))
           (unsafe-fl* sf (unsafe-fl+ (unsafe-fl+ (unsafe-fl+ ln-term -0.6875) x) r))))
        ((unsafe-fl<= x 4.0)
         (let ((s (unsafe-fl*
                   (unsafe-fl/ 1.0 x)
                   (if scale 1.0 (unsafe-flexp (unsafe-fl- 0.0 x)))))
               (r (unchecked-chebyshev-eval
                   AE13-cs (unsafe-fl/ (unsafe-fl- (unsafe-fl/ 8.0 x) 5.0) 3.0))))
           (unsafe-fl* s (unsafe-fl+ 1.0 r))))
        ((or (unsafe-fl<= x xmax)
             scale)
         (let ((s (unsafe-fl*
                   (unsafe-fl/ 1.0 x)
                   (if scale 1.0 (unsafe-flexp (unsafe-fl- 0.0 x)))))
               (r (unchecked-chebyshev-eval
                   AE14-cs (unsafe-fl- (unsafe-fl/ 8.0 x) 1.0))))
           (unsafe-fl* s (unsafe-fl+ 1.0 r))))
        (else
         0.0)))

(define (expint-E2-impl x scale)
  (cond ((and (unsafe-fl< x (unsafe-fl- 0.0 xmax))
              (not scale))
         +inf.0)
        ((unsafe-fl< x 100.0)
         (let ((ex (if scale 1.0 (unsafe-flexp (unsafe-fl- 0.0 x))))
               (r (expint-E1-impl x scale)))
           (unsafe-fl- ex (unsafe-fl* x r))))
        ((or (unsafe-fl< x xmax)
             scale)
         (let* ((s (if scale 1.0 (unsafe-flexp (unsafe-fl- 0.0 x))))
                (c1  -2.0)
                (c2   6.0)
                (c3  -24.0)
                (c4   120.0)
                (c5  -720.0)
                (c6   5040.0)
                (c7  -40320.0)
                (c8   362880.0)
                (c9  -3628800.0)
                (c10  39916800.0)
                (c11 -479001600.0)
                (c12  6227020800.0)
                (c13 -87178291200.0)
                (y (unsafe-fl/ 1.0 x))
                (sum9 (unsafe-fl+
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
                             (unsafe-fl* y c13)))))))))
                (sum5 (unsafe-fl+
                       c5
                       (unsafe-fl*
                        y
                        (unsafe-fl+
                         c6
                         (unsafe-fl*
                          y
                          (unsafe-fl+
                           c7
                           (unsafe-fl*
                            y
                            (unsafe-fl+
                             c8
                             (unsafe-fl* y sum9)))))))))
                (sum (unsafe-fl*
                      y
                      (unsafe-fl+
                       c1
                       (unsafe-fl*
                        y
                        (unsafe-fl+
                         c2
                         (unsafe-fl*
                          y
                          (unsafe-fl+
                           c3
                           (unsafe-fl*
                            y
                            (unsafe-fl+
                             c4
                             (unsafe-fl* y sum5)))))))))))
           (unsafe-fl* s (unsafe-fl/ (unsafe-fl+ 1.0 sum) x))))
        (else
         0.0)))

(define (expint-E1 x)
  (with-float (x)
    (expint-E1-impl x #f)))

(define (expint-E1-scaled x)
  (with-float (x)
    (expint-E1-impl x #t)))

(define (expint-E2 x)
  (with-float (x)
    (expint-E2-impl x #f)))

(define (expint-E2-scaled x)
  (with-float (x)
    (expint-E2-impl x #t)))

(define (expint-Ei x)
  (with-float (x)
    (- (expint-E1 (- x)))))

(define (expint-Ei-scaled x)
  (with-float (x)
    (- (expint-E1-scaled (- x)))))

;;; Module Contracts

(provide
 (rename-out (expint-E1 unchecked-expint-E1)
             (expint-E1-scaled unchecked-expint-E1-scaled)
             (expint-E2 unchecked-expint-E2)
             (expint-E2-scaled unchecked-expint-E2-scaled)
             (expint-Ei unchecked-expint-Ei)
             (expint-Ei-scaled unchecked-expint-Ei-scaled)))

(provide/contract
 (expint-E1
  (-> real? real?))
 (expint-E1-scaled
  (-> real? real?))
 (expint-E2
  (-> real? real?))
 (expint-E2-scaled
  (-> real? real?))
 (expint-Ei
  (-> real? real?))
 (expint-Ei-scaled
  (-> real? real?)))
