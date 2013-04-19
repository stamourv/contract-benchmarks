#lang racket
;;; Science Collection
;;; histogram.rkt
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
;;; This code implements histograms for the PLT Scheme Science
;;; Collection.  It is loosely based on the histograms provided by the
;;; GNU Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 0.1.0    08/06/04  This is the initial release of the histogram
;;;                    routines. (Doug Williams)
;;; 0.2.0    08/28/04  Added make-histogram-with-ranges-uniform and
;;;                    improved the histogram-increment! and 
;;;                    histogram-accumulate! more efficient by
;;;                    computing the bin for histograms with uniform
;;;                    ranges.  (Doug Williams)
;;; 1.0.0    09/28/04  Added contracts for functions.  Marked as ready
;;;                    for Release 1.0.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and getting
;;;                    ready for PLT Scheme V4.0.  (Doug Williams)
;;; 2.0.1    01/28/08  Fixed void? contract problem.  (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changed (when (not ...) ...) to
;;;                    (unless ... ...).  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

;;; Data definition
;;;
;;; Histogram structure with 4 fields:
;;;   0 - n, number of bins
;;;   1 - ranges, vector defining the bin ranges (length is n+1)
;;;   2 - bins, vector containing the bin values
;;;   3 - ranges-uniform?, boolean

(define-values (struct:histogram
                histogram-constructor
                histogram?
                histogram-field-ref
                set-histogram-field!)
  (make-struct-type 'histogram #f 4 0))


;;; make-histogram: integer -> histogram
;;; The procedure make-histogram returns a new histogram with the
;;; specified number of bins.
(define (make-histogram n)
  (unless (> n 0)
    (error 'make-histogram 
           "number of bins must be greater than 0"))
  (histogram-constructor n 
                         (make-vector (+ n 1)) 
                         (make-vector n)
                         #f))

;;; make-histogram-with-ranges-uniform: integer x real x real ->
;;;                                     histogram
;;; This function returns a histogram with the specified number
;;; of uniform sized bins over the specified range.
(define (make-histogram-with-ranges-uniform n min max)
  (unless (> n 0)
    (error 'make-histogram-with-ranges-uniform
           "number of bins must be greater than 0"))
  (let ((h (make-histogram n)))
    (set-histogram-ranges-uniform! h min max)
    h))

;;; histogram-n: histogram -> integer
(define histogram-n
  (make-struct-field-accessor histogram-field-ref 0 'n))

;;; histogram-ranges: histogram -> vector
(define histogram-ranges
  (make-struct-field-accessor histogram-field-ref 1 'ranges))

;;; set-histogram-ranges: histogram x vector -> void
(define (set-histogram-ranges! h ranges)
  (unless (= (vector-length ranges)
                (+ (histogram-n h) 1))
    (error 'set-histogram-ranges
           "size of ranges vector must match size of histogram"))
  ;; Set ranges.
  (do ((i 0 (+ i 1))) 
      ((> i (histogram-n h)) (void))
    (vector-set! (histogram-ranges h) i
                 (vector-ref ranges i)))
  ;; Re-initialize the bins.
  (do ((i 0 (+ i 1)))
      ((= i (histogram-n h)) (void))
    (vector-set! (histogram-bins h) i 0))
  ;; Clear ranges-uniform?
  (set-histogram-ranges-uniform?! h #f)
  (void))

;;; set-histogram-ranges-uniform!: histogram x real x real -> void
(define (set-histogram-ranges-uniform! h min max)
  (let* ((n (histogram-n h))
         (ranges (histogram-ranges h))
         (bin-size (/ (- max min) n)))
    (do ((i 0 (+ i 1)))
        ((= i n) (void))
      (vector-set! ranges i (+ min (* i bin-size))))
    (vector-set! ranges n max))
  ;; Re-initialize the bins.
  (do ((i 0 (+ i 1)))
      ((= i (histogram-n h)) (void))
    (vector-set! (histogram-bins h) i 0))
  ;; Set uniform-ranges?
  (set-histogram-ranges-uniform?! h #t)
  (void))

;;; histogram-bins: histogram -> vector
(define histogram-bins
  (make-struct-field-accessor histogram-field-ref 2 'bins))

;;; histogram-ranges-uniform?: histogram -> boolean
(define histogram-ranges-uniform?
  (make-struct-field-accessor histogram-field-ref 3
                              'ranges-uniform?))

;;; set-histogram-ranges-uniform?! histogram x boolean -> void
(define set-histogram-ranges-uniform?!
  (make-struct-field-mutator set-histogram-field! 3
                             'ranges-uniform?))

;;; histogram-increment!: histogram x real -> void
;;; Increment the bin corresponding to the x value by one.
(define (histogram-increment! h x)
  (let ((n (histogram-n h))
        (ranges (histogram-ranges h))
        (bins (histogram-bins h))
        (uniform-ranges? (histogram-ranges-uniform? h)))
    (if uniform-ranges?
        ;; Compute bin
        (let ((i (inexact->exact
                  (floor (/ (- x (vector-ref ranges 0))
                            (/ (- (vector-ref ranges n)
                                  (vector-ref ranges 0))
                               n))))))
          (when (<= 0 i (- n 1))
            (vector-set! bins i
                         (+ (vector-ref bins i) 1))))
        ;; Search for bin
        (let/ec exit
          (when (< x (vector-ref ranges 0))
            (exit (void)))
          (do ((i 0 (+ i 1)))
              ((= i n) (void))
            (when (< x (vector-ref ranges (+ i 1)))
              (vector-set! bins i
                           (+ (vector-ref bins i) 1))
              (exit (void))))))))

;;; histogram-accumulate!: histogram x real x real -> void
;;; Increment the bin corresponding to the x value by the weight.
(define (histogram-accumulate! h x weight)
  (let ((n (histogram-n h))
        (ranges (histogram-ranges h))
        (bins (histogram-bins h))
        (uniform-ranges? (histogram-ranges-uniform? h)))
    (if uniform-ranges?
        ;; Compute bin
        (let ((i (inexact->exact
                  (floor (/ (- x (vector-ref ranges 0))
                            (/ (- (vector-ref ranges n)
                                  (vector-ref ranges 0))
                               n))))))
          (when (<= 0 i (- n 1))
            (vector-set! bins i
                         (+ (vector-ref bins i) weight))))
        ;; Search for bin
        (let/ec exit
          (when (< x (vector-ref ranges 0))
            (exit (void)))
          (do ((i 0 (+ i 1)))
              ((= i n) (void))
            (when (< x (vector-ref ranges (+ i 1)))
              (vector-set! bins i
                           (+ (vector-ref bins i) weight))
              (exit (void))))))))

;;; histogram-get: histogram x integer -> real
(define (histogram-get h i)
  (vector-ref (histogram-bins h) i))

;;; histogram-range: histogram x integer -> real x real
(define (histogram-get-range h i)
  (values (vector-ref (histogram-ranges h) i)
          (vector-ref (histogram-ranges h) (+ i 1))))
;;; Histogram Statistics

;;; histogram-max: histogram -> real
(define (histogram-max h)
  (let ((n (histogram-n h))
        (bins (histogram-bins h))
        ;; Initialize max to the first bin value
        (max (vector-ref (histogram-bins h) 0)))
    (do ((i 0 (+ i 1)))
        ((= i n) max)
      (when (> (vector-ref bins i) max)
        (set! max (vector-ref bins i))))))

;;; histogram-min: histogram -> real
(define (histogram-min h)
  (let ((n (histogram-n h))
        (bins (histogram-bins h))
        ;; Initialize min to the first bin value.
        (min (vector-ref (histogram-bins h) 0)))
    (do ((i 0 (+ i 1)))
        ((= i n) min)
      (when (< (vector-ref bins i) min)
        (set! min (vector-ref bins i))))))

;;; histogram-mean: histogram -> real
(define (histogram-mean h)
  (let ((n (histogram-n h))
        (ranges (histogram-ranges h))
        (bins (histogram-bins h))
        (wmean 0.0)
        (W 0.0))
    (do ((i 0 (+ i 1)))
        ((= i n) wmean)
      (let ((xi (/ (+ (vector-ref ranges (+ i 1))
                      (vector-ref ranges i))
                   2.0))
            (wi (vector-ref bins i)))
        (when (> wi 0.0)
          (set! W (+ W wi))
          (set! wmean (+ wmean (* (- xi wmean) (/ wi W)))))))))

;;; histogram-sigma: histogram -> real
(define (histogram-sigma h)
  (let ((n (histogram-n h))
        (ranges (histogram-ranges h))
        (bins (histogram-bins h))
        (wvariance 0.0)
        (wmean 0.0)
        (W 0))
    (do ((i 0 (+ i 1)))
        ((= i n) wmean)
      (let ((xi (/ (+ (vector-ref ranges (+ i 1))
                      (vector-ref ranges i))
                   2.0))
            (wi (vector-ref bins i)))
        (when (> wi 0.0)
          (set! W (+ W wi))
          (set! wmean (+ wmean (* (- xi wmean) (/ wi W)))))))
    (set! W 0.0)
    (do ((i 0 (+ i 1)))
        ((= i n) (sqrt wvariance))
      (let ((xi (/ (+ (vector-ref ranges (+ i 1))
                      (vector-ref ranges i))
                   2.0))
            (wi (vector-ref bins i)))
        (when (> wi 0.0)
          (let ((delta (- xi wmean)))
            (set! W (+ W wi))
            (set! wvariance (+ wvariance 
                               (* (- (* delta delta) wvariance)
                                  (/ wi W))))))))))

;;; histogram-sum: histogram -> real
(define (histogram-sum h)
  (let ((n (histogram-n h))
        (bins (histogram-bins h))
        (sum 0.0))
    (do ((i 0 (+ i 1)))
        ((= i n) sum)
      (set! sum (+ sum (vector-ref bins i))))))
;;; Contracts

(provide
 (rename-out (histogram-increment! unchecked-histogram-increment!)
             (histogram-accumulate! unchecked-histogram-accumulate!)))

(provide/contract
 (histogram?
  (-> any/c boolean?))
 (make-histogram
  (-> (and/c integer? (>=/c 1)) histogram?))
 (make-histogram-with-ranges-uniform
  (->i ((n (and/c integer? (>=/c 1)))
        (min real?)
        (max (min) (>/c min)))
       (result () histogram?)))
 (histogram-n
  (-> histogram? (and/c integer? (>=/c 1))))
 (histogram-ranges
  (-> histogram? (vectorof real?)))
 (set-histogram-ranges!
  (-> histogram? (vectorof real?) void?))
 (set-histogram-ranges-uniform!
  (->i ((h histogram?)
        (min real?)
        (max (min) (>/c min)))
       (result () void?)))
 (histogram-bins
  (-> histogram? (vectorof real?)))
 (histogram-increment!
  (-> histogram? real? void?))
 (histogram-accumulate!
  (-> histogram? real? (>=/c 0.0) void?))
 (histogram-get
  (-> histogram? natural-number/c (>=/c 0.0)))
 (histogram-get-range
  (-> histogram? natural-number/c (values real? real?)))
 (histogram-max
  (-> histogram? (>=/c 0.0)))
 (histogram-min
  (-> histogram? (>=/c 0.0)))
 (histogram-mean
  (-> histogram? real?))
 (histogram-sigma
  (-> histogram? (>=/c 0.0)))
 (histogram-sum
  (-> histogram? (>=/c 0.0))))
