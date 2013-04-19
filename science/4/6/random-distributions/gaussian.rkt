#lang racket
;;; Science Collection
;;; random-distributions/gaussian.rkt
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
;;; This code in based on the Random Number Distributions in the GNU
;;; Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 0.9.0    08/05/04  This is the initial release of the guassion
;;;                    distribution routines ported from GSL. (Doug
;;;                    Williams)
;;; 0.9.1    08/07/04  Added cummulative density functions. (Doug
;;;                    Williams)
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added
;;;                    contracts for functions.  (Doug Williams)
;;; 1.0.1    02/07/06  Reimplemented the cdf functions with the GSL
;;;                    routines.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/11/11  Added the cdf-Q function. (MDW)

(require "../machine.ss")
(require "../math.ss")
(require "../random-source.ss")

;;; Gaussian (normal) distribution

;;; random-gaussian: random-source x real x real -> real
;;; random-gaussian: real x real -> real
;;; Polar (Box-Muller) method; see Knuth v2, 3rd ed. p122
(define random-gaussian
  (case-lambda
    ((r mu sigma)
     (let ((x 0.0)
           (y 0.0)
           (r2 0.0))
       (let loop ()
         ;; Choose x,y in uniform square (-1,-1) to (+1, +1)
         (set! x (+ -1.0 (* 2.0 (unchecked-random-uniform r))))
         (set! y (+ -1.0 (* 2.0 (unchecked-random-uniform r))))
         ;; See if it is in the unit circle
         (set! r2 (+ (* x x) (* y y)))
         ;; Note: since neither x not y can = 0.0, r2 > 0.0
         (when (> r2 1.0)
           (loop)))
       ;; Box-Muller transform
       (+ mu (* sigma y (sqrt (/ (* -2.0 (log r2)) r2))))))
    ((mu sigma)
     (random-gaussian (current-random-source) mu sigma))))

;;; random-unit-gaussian: random-source -> real
;;; random-unit-gaussian: -> real
(define random-unit-gaussian
  (case-lambda
    ((r)
     (random-gaussian r 0.0 1.0))
    (()
     (random-unit-gaussian (current-random-source)))))

;;; random-gaussian-ratio-method: ransom-source x real x real -> real
;;; random-gaussian-ratio-method: real x real -> real
;;; Ratio method (Kinderman-Monahan)
(define random-gaussian-ratio-method
  (case-lambda
    ((r mu sigma)
     (let ((u 0.0)
           (v 0.0)
           (x 0.0))
       (let loop ()
         (set! v (unchecked-random-uniform r))
         (set! u (unchecked-random-uniform r))
         ;; Note: u > 0.0
         ;; Const 1.715... = sqrt(8/e)
         (set! x (/ (* 1.71552776992141359295 (- v 0.5)) u))
         (when (> (* x x)
                  (* -4.0 (log u)))
           (loop)))
       (+ mu (* sigma x))))
    ((mu sigma)
     (random-gaussian-ratio-method (current-random-source) mu sigma))))

;;; random-unit-gaussian-ratio-method: random-source -> real
;;; random-unit-gaussian-ratio-method: -> real
(define random-unit-gaussian-ratio-method
  (case-lambda
    ((r)
     (random-gaussian-ratio-method r 0.0 1.0))
    (()
     (random-unit-gaussian-ratio-method (current-random-source)))))

;;; gaussian-pdf: real x real x real -> real
;;; This function computes the probability density p(x) at x for a
;;; gaussian distribution with mean mu and standard deviation sigma.
(define (gaussian-pdf x mu sigma)
  (* (/ 1.0 (* sigma (sqrt (* 2.0 pi))))
     (exp (/ (- (* (- x mu) (- x mu)))
             (* 2.0 sigma sigma)))))

;;; unit-gaussian-pdf: real -> real
(define (unit-gaussian-pdf x)
  (gaussian-pdf x 0.0 1.0))

;;; cdf implementation

;;; IEEE double precision dependant constants
;;; gauss-epsilon: smallest positive value such that
;;;                (gaussian-cdf x) > 0.5
;;; gauss-xupper: largest value of x such that
;;;               (gaussian-cdf x) < 1.0
;;; gauss-xlower: smallest value of x such that
;;;               (gaussian-cdf x) > 0.0
(define gauss-epsilon (/ double-epsilon 2.0))
(define gauss-xupper 8.572)
(define gauss-xlower -37.519)

(define gauss-scale 16.0)

(define (get-del x rational)
  (let ((xsq (/ (floor (* x gauss-scale)) gauss-scale))
        (del 0.0))
    (set! del (* (- x xsq) (+ x xsq)))
    (set! del (* del 0.5))
    (* (exp (* -0.5 xsq xsq)) (exp (* -1.0 del)) rational)))

;;; Normal cdf for |x| < 0.66291
(define (gauss-small x)
  (define a '#(2.2352520354606839287
               161.02823106855587881
               1067.6894854603709582
               18154.981253343561249
               0.065682337918207449113))
  (define b '#(47.20258190468824187
               976.09855173777669322
               10260.932208618978205
               45507.789335026729956))
  (let* ((xsq (* x x))
         (xnum (* (vector-ref a 4) xsq))
         (xden xsq))
    (do ((i 0 (+ i 1)))
        ((= i 3) (/ (* x (+ xnum (vector-ref a 3)))
                    (+ xden (vector-ref b 3))))
      (set! xnum (* (+ xnum (vector-ref a i)) xsq))
      (set! xden (* (+ xden (vector-ref b i)) xsq)))))

;;; Normal cdf for 0.66291 < |x| < sqrt(32)
(define (gauss-medium x)
  (define c '#(0.39894151208813466764
               8.8831497943883759412
               93.506656132177855979
               597.27027639480026226
               2494.5375852903726711
               6848.1904505362823326
               11602.651437647350124
               9842.7148383839780218
               1.0765576773720192317e-8))
  (define d '#(22.266688044328115691
               235.38790178262499861
               1519.377599407554805
               6485.558298266760755
               18615.571640885098091
               34900.952721145977266
               38912.003286093271411
               19685.429676859990727))
  (let* ((absx (abs x))
         (xnum (* (vector-ref c 8) absx))
         (xden absx)
         (temp 0.0))
    (do ((i 0 (+ i 1)))
        ((= i 7) (void))
      (set! xnum (* (+ xnum (vector-ref c i)) absx))
      (set! xden (* (+ xden (vector-ref d i)) absx)))
    (set! temp (/ (+ xnum (vector-ref c 7))
                  (+ xden (vector-ref d 7))))
    (get-del x temp)))

;;; Normal cdf for sqrt(32) < x < gauss-xupper U
;;;                gauss-xlower < x < - sqrt(32)
(define (gauss-large x)
  (define p '#(0.21589853405795699
               0.1274011611602473639
               0.022235277870649807
               0.001421619193227893466
               2.9112874951168792e-5
               0.02307344176494017303))
  (define q '#(1.28426009614491121
               0.468238212480865118
               0.0659881378689285515
               0.00378239633202758244
               7.29751555083966205e-5))
  (let* ((absx (abs x))
         (xsq (/ 1.0 (* x x)))
         (xnum (* (vector-ref p 5) xsq))
         (xden xsq)
         (temp 0.0))
    (do ((i 0 (+ i 1)))
        ((= i 4)(void))
      (set! xnum (* (+ xnum (vector-ref p i)) xsq))
      (set! xden (* (+ xden (vector-ref q i)) xsq)))
    (set! temp (/ (* xsq (+ xnum (vector-ref p 4)))
                  (+ xden (vector-ref q 4))))
    (set! temp (/ (- (/ sqrt1/2 sqrtpi) temp) absx))
    (get-del x temp)))

;;; unit-gaussian-cdf: real -> real
(define (unit-gaussian-cdf-P x)
  (let ((absx (abs x)))
    (cond ((< absx gauss-epsilon)
           0.5)
          ((< absx 0.66291)
           (+ 0.5 (gauss-small x)))
          ((< absx (sqrt 32.0))
           (let ((result (gauss-medium x)))
             (if (> x 0.0)
                 (- 1.0 result)
                 result)))
          ((> x gauss-xupper)
           1.0)
          ((< x gauss-xlower)
           0.0)
          (else
           (let ((result (gauss-large x)))
             (if (> x 0.0)
                 (- 1.0 result)
                 result))))))

(define (unit-gaussian-cdf-Q x)
  (let ((absx (abs x)))
    (cond ((< absx gauss-epsilon)
           0.5)
          ((< absx 0.66291)
           (let ((result (gauss-small x)))
             (if (< x 0.0)
                 (+ (abs result) 0.5)
                 (- 0.5 result))))
          ((< absx (sqrt 32.0))
           (let ((result (gauss-medium x)))
             (if (< x 0.0)
                 (- 1.0 result)
                 result)))
          ((> x (- gauss-xlower))
           0.0)
          ((< x (- gauss-xupper))
           1.0)
          (else
           (let ((result (gauss-large x)))
             (if (< x 0.0)
                 (- 1.0 result)
                 result))))))

(define unit-gaussian-cdf unit-gaussian-cdf-P)

;;; gaussian-cdf: real x real -> real
;;; This function computes the cummulative density d(x) at x for a
;;; gaussian distribution with mean mu and standard deviation sigma.
(define (gaussian-cdf-P x mu sigma)
  (unit-gaussian-cdf-P (/ (- x mu) sigma)))

(define (gaussian-cdf-Q x mu sigma)
  (unit-gaussian-cdf-Q (/ (- x mu) sigma)))

(define gaussian-cdf gaussian-cdf-P)

(provide
 (rename-out (random-gaussian unchecked-random-gaussian)
             (random-unit-gaussian unchecked-random-unit-gaussian)
             (random-gaussian-ratio-method unchecked-random-gaussian-ratio-method)
             (random-unit-gaussian-ratio-method unchecked-random-unit-gaussian-ratio-method)
             (gaussian-pdf unchecked-gaussian-pdf)
             (unit-gaussian-pdf unchecked-unit-gaussian-pdf)
             (gaussian-cdf-P unchecked-gaussian-cdf-P)
             (gaussian-cdf-Q unchecked-gaussian-cdf-Q)
             (gaussian-cdf unchecked-gaussian-cdf)
             (unit-gaussian-cdf-P unchecked-unit-gaussian-cdf-P)
             (unit-gaussian-cdf-Q unchecked-unit-gaussian-cdf-Q)
             (unit-gaussian-cdf unchecked-unit-gaussian-cdf)))

(provide/contract
 (random-gaussian
  (case-> (-> random-source? real? (>=/c 0.0) real?)
          (-> real? (>=/c 0.0) real?)))
 (random-unit-gaussian
  (case-> (-> random-source? real?)
          (-> real?)))
 (random-gaussian-ratio-method
  (case-> (-> random-source? real? (>=/c 0.0) real?)
          (-> real? (>=/c 0.0) real?)))
 (random-unit-gaussian-ratio-method
  (case-> (-> random-source? real?)
          (-> real?)))
 (gaussian-pdf
  (-> real? real? (>=/c 0.0) (>=/c 0.0)))
 (unit-gaussian-pdf
  (-> real? (>=/c 0.0)))
 (gaussian-cdf-P
  (-> real? real? (>=/c 0.0) (real-in 0.0 1.0)))
 (gaussian-cdf-Q
  (-> real? real? (>=/c 0.0) (real-in 0.0 1.0)))
 (gaussian-cdf
  (-> real? real? (>=/c 0.0) (real-in 0.0 1.0)))
 (unit-gaussian-cdf-P
  (-> real? (real-in 0.0 1.0)))
 (unit-gaussian-cdf-Q
  (-> real? (real-in 0.0 1.0)))
 (unit-gaussian-cdf
  (-> real? (real-in 0.0 1.0))))
