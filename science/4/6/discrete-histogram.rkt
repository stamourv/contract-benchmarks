#lang racket
;;; Science Collection
;;; discrete-histogram.rkt
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
;;; This module provides discrete histograms - that is, the range of
;;; the histogram (bins) must be integers.
;;;
;;; Version  Date      Description
;;  1.0.0    09/30/04  Marked as ready for Release 1.0. (MDW)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme V4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 3.0.1    07/01/08  Changed (while (not ...) ...) to (unless ... ...). (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)
  
;;; Data Definition

;;; Structure: discrete-histogram
(define-values (struct:discrete-histogram
                discrete-histogram-constructor
                discrete-histogram?
                discrete-histogram-field-ref
                set-discrete-histogram-field!)
  (make-struct-type 'discrete-histogram #f 4 0))

;;; make-discrete-histogram: integer x integer x boolean -> discrete-histogram
(define make-discrete-histogram
  (case-lambda
    ((n1 n2 dynamic?)
     (unless (<= n1 (+ n2 1))
       (error 'make-discrete-histogram
              "illegal range ~a .. ~a" n1 n2))
     (discrete-histogram-constructor 
      n1 n2 dynamic? (make-vector (+ (- n2 n1) 1))))
    ((n1 n2)
     (make-discrete-histogram n1 n2 #f))
    (()
     (make-discrete-histogram 0 -1 #t))))

;;; Define discrete-histogram fields
(define discrete-histogram-n1
  (make-struct-field-accessor discrete-histogram-field-ref 0 'n1))

(define set-discrete-histogram-n1!
  (make-struct-field-mutator set-discrete-histogram-field! 0 'n1))

(define discrete-histogram-n2
  (make-struct-field-accessor discrete-histogram-field-ref 1 'n2))

(define set-discrete-histogram-n2!
  (make-struct-field-mutator set-discrete-histogram-field! 1 'n2))

(define discrete-histogram-dynamic?
  (make-struct-field-accessor discrete-histogram-field-ref 2 'dynamic?))

(define discrete-histogram-bins
  (make-struct-field-accessor discrete-histogram-field-ref 3 'bins))

(define set-discrete-histogram-bins!
  (make-struct-field-mutator set-discrete-histogram-field! 3 'bins))

;;; discrete-histogram-increment!:
;;;   discrete-histogram x integer -> void
(define (discrete-histogram-increment! h i)
  (discrete-histogram-accumulate! h i 1))

;; discrete-histogram-accumulate!:
;;   discrete-histogram x integer x real -> void
(define (discrete-histogram-accumulate! h i weight)
  (let* ((n1 (discrete-histogram-n1 h))
         (n2 (discrete-histogram-n2 h))
         (n (+ (- n2 n1) 1))
         (dynamic? (discrete-histogram-dynamic? h))
         (bins (discrete-histogram-bins h)))
    (let/ec exit
      (unless (<= n1 i n2)
        (if (not dynamic?)
            (exit (void))
            (let* ((new-n1 (if (> n 0) (min i n1) i))
                   (new-n2 (if (> n 0) (max i n2) i))
                   (new-n (+ (- new-n2 new-n1) 1))
                   (new-bins (make-vector new-n)))
              ;; Copy bins
              (do ((i 0 (+ i 1)))
                  ((= i n) (void))
                (vector-set! new-bins (+ (- n1 new-n1) i)
                             (vector-ref bins i)))
              ;; Update local variables
              (set! n1 new-n1)
              (set! n2 new-n2)
              (set! n new-n)
              (set! bins new-bins)
              ;; Update discrete-histogram instance
              (set-discrete-histogram-n1! h n1)
              (set-discrete-histogram-n2! h n2)
              (set-discrete-histogram-bins! h bins))))
      ;; Update bins (<= n1 i n2)
      (let ((bin (- i n1)))
        (vector-set! bins bin (+ (vector-ref bins bin) weight))))))

;;; discrete-histogram-get: discrete-histogram x integer -> real
(define (discrete-histogram-get h i)
  (let ((n1 (discrete-histogram-n1 h))
        (n2 (discrete-histogram-n2 h))
        (bins (discrete-histogram-bins h)))
    (if (<= n1 i n2)
        (vector-ref bins (- i n1))
        (raise-mismatch-error 'discrete-histogram-bins
                              "index out of range" i))))

;;; discrete-histogram-max: discrete-histogram -> real
(define (discrete-histogram-max h)
  (let* ((n1 (discrete-histogram-n1 h))
         (n2 (discrete-histogram-n2 h))
         (n (+ (- n2 n1) 1))
         (bins (discrete-histogram-bins h))
         (max -inf.0))
    (do ((i 0 (+ i 1)))
        ((>= i n) max)
      (let ((x (vector-ref bins i)))
        (when (> x max)
          (set! max x))))))

;;; discrete-histogram-min: discrete-histogram -> real
(define (discrete-histogram-min h)
  (let* ((n1 (discrete-histogram-n1 h))
         (n2 (discrete-histogram-n2 h))
         (n (+ (- n2 n1) 1))
         (bins (discrete-histogram-bins h))
         (min +inf.0))
    (do ((i 0 (+ i 1)))
        ((>= i n) min)
      (let ((x (vector-ref bins i)))
        (when (< x min)
          (set! min x))))))

;;; discrete-histogram-mean: histogram -> real
(define (discrete-histogram-mean h)
  (let* ((n1 (discrete-histogram-n1 h))
         (n2 (discrete-histogram-n2 h))
         (n (+ (- n2 n1) 1))
         (bins (discrete-histogram-bins h))
         (wmean 0.0)
         (W 0.0))
    (do ((i 0 (+ i 1)))
        ((= i n) wmean)
      (let ((xi (+ n1 i))
            (wi (vector-ref bins i)))
        (when (> wi 0.0)
          (set! W (+ W wi))
          (set! wmean (+ wmean (* (- xi wmean) (/ wi W)))))))))

;;; discrete-histogram-sigma: histogram -> real
(define (discrete-histogram-sigma h)
  (let* ((n1 (discrete-histogram-n1 h))
         (n2 (discrete-histogram-n2 h))
         (n (+ (- n2 n1) 1))
         (bins (discrete-histogram-bins h))
         (wvariance 0.0)
         (wmean 0.0)
         (W 0))
    (do ((i 0 (+ i 1)))
        ((= i n) wmean)
      (let ((xi (+ n1 i))
            (wi (vector-ref bins i)))
        (when (> wi 0.0)
          (set! W (+ W wi))
          (set! wmean (+ wmean (* (- xi wmean) (/ wi W)))))))
    (set! W 0.0)
    (do ((i 0 (+ i 1)))
        ((= i n) (sqrt wvariance))
      (let ((xi (+ n1 i))
            (wi (vector-ref bins i)))
        (when (> wi 0.0)
          (let ((delta (- xi wmean)))
            (set! W (+ W wi))
            (set! wvariance (+ wvariance 
                               (* (- (* delta delta) wvariance)
                                  (/ wi W))))))))))

;;; discrete-histogram-sum: discrete-histogram -> real
(define (discrete-histogram-sum h)
  (let* ((n1 (discrete-histogram-n1 h))
         (n2 (discrete-histogram-n2 h))
         (n (+ (- n2 n1) 1))
         (bins (discrete-histogram-bins h))
         (sum 0.0))
    (do ((i 0 (+ i 1)))
        ((>= i n) sum)
      (set! sum (+ sum (vector-ref bins i))))))

;;; Module Contracts

(provide
 (rename-out (discrete-histogram-increment! unchecked-discrete-histogram-increment!)
             (discrete-histogram-accumulate! unchecked-discrete-histogram-accumumate!)))

(provide/contract
 (discrete-histogram?
  (-> any/c boolean?))
 (make-discrete-histogram
  (case-> (-> exact-integer? exact-integer? boolean? discrete-histogram?)
          (-> exact-integer? exact-integer? discrete-histogram?)
          (-> discrete-histogram?)))
 (discrete-histogram-n1
  (-> discrete-histogram? exact-integer?))
 (discrete-histogram-n2
  (-> discrete-histogram? exact-integer?))
 (discrete-histogram-dynamic?
  (-> discrete-histogram? boolean?))
 (discrete-histogram-bins
  (-> discrete-histogram? (vectorof real?)))
 (discrete-histogram-increment!
  (-> discrete-histogram? exact-integer? void?))
 (discrete-histogram-accumulate!
  (-> discrete-histogram? exact-integer? real? void?))
 (discrete-histogram-get
  (-> discrete-histogram? exact-integer? real?))
 (discrete-histogram-max
  (-> discrete-histogram? real?))
 (discrete-histogram-min 
  (-> discrete-histogram? real?))
 (discrete-histogram-mean
  (-> discrete-histogram? real?))
 (discrete-histogram-sigma
  (-> discrete-histogram? (>=/c 0.0)))
 (discrete-histogram-sum
  (-> discrete-histogram? real?)))
