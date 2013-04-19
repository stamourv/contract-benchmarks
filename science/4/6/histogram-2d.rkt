#lang racket
;;; Science Collection
;;; histogram-2d.rkt
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
;;; This module implements 2D histograms.  It is based on the
;;; Histograms in the GNU Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 1.0.0    09/29/04  Marked as ready for Release 1.0.  (Doug
;;;                    Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and getting
;;;                    ready for PLT Scheme V4.0.  (Doug Williams)
;;; 2.0.1    01/28/08  Fixed void? contract problem.  (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/15/11  Changed the header and restructured the code. (MDW)

;;; Data Definition

;;; Histogram structure with 6 fields:
;;;   0 - nx, number of bins in the x direction
;;;   1 - ny, number of bins in the y direction
;;;   2 - x-ranges 
;;;   3 - y-ranges
;;;   4 - bins
;;;   5 - ranges-uniform?

(define-values (struct:histogram-2d
                histogram-2d-constructor
                histogram-2d?
                histogram-2d-field-ref
                set-histogram-2d-field!)
  (make-struct-type 'histogram-2d #f 6 0))

;;; make-histogram-2d: integer x integer -> histogram
(define (make-histogram-2d nx ny)
  (histogram-2d-constructor
   nx ny
   (make-vector (+ nx 1))
   (make-vector (+ ny 1))
   (make-vector (* nx ny))
   #f))

(define (make-histogram-2d-with-ranges-uniform
         nx ny x-min x-max y-min y-max)
  (let ((h (make-histogram-2d nx ny)))
    (set-histogram-2d-ranges-uniform! h x-min x-max y-min y-max)
    h))

(define histogram-2d-nx
  (make-struct-field-accessor histogram-2d-field-ref 0 'nx))

(define histogram-2d-ny
  (make-struct-field-accessor histogram-2d-field-ref 1 'ny))

(define histogram-2d-x-ranges
  (make-struct-field-accessor histogram-2d-field-ref 2 'x-ranges))

(define histogram-2d-y-ranges
  (make-struct-field-accessor histogram-2d-field-ref 3 'y-ranges))

(define (set-histogram-2d-ranges! h x-ranges y-ranges)
  (let ((nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (x-ranges (histogram-2d-x-ranges h))
        (y-ranges (histogram-2d-y-ranges h))
        (bins (histogram-2d-bins h)))
    ;; Set x ranges
    (do ((i 0 (+ i 1)))
        ((> i nx) (void))
      (vector-set! x-ranges i
                   (vector-ref x-ranges i)))
    ;; Set y ranges
    (do ((i 0 (+ i 1)))
        ((> i ny) (void))
      (vector-set! y-ranges i
                   (vector-ref y-ranges i)))
    ;; Re-initialize the bins
    (do ((i 0 (+ i 1)))
        ((= i (* nx ny)) (void))
      (vector-set! bins i 0))
    ;; Clear ranges-uniform?
    (set-histogram-2d-ranges-uniform?! h #f)
    (void)))

(define (set-histogram-2d-ranges-uniform!
         h x-min x-max y-min y-max)
  (let* ((nx (histogram-2d-nx h))
         (ny (histogram-2d-ny h))
         (x-ranges (histogram-2d-x-ranges h))
         (y-ranges (histogram-2d-y-ranges h))
         (bins (histogram-2d-bins h))
         (x-bin-width (/ (- x-max x-min) nx))
         (y-bin-width (/ (- y-max y-min) ny)))
    ;; Set x ranges
    (do ((i 0 (+ i 1)))
        ((= i nx) (void))
      (vector-set! x-ranges i (+ x-min (* i x-bin-width))))
    (vector-set! x-ranges nx x-max)
    ;; Set y ranges
    (do ((i 0 (+ i 1)))
          ((= i ny) (void))
      (vector-set! y-ranges i (+ y-min (* i y-bin-width))))
    (vector-set! y-ranges ny y-max)
    ;; Re-initialize the bins
    (do ((i 0 (+ i 1)))
          ((= i (* nx ny)) (void))
      (vector-set! bins i 0))
    ;; Set uniform-ranges?
    (set-histogram-2d-ranges-uniform?! h #t)
    (void)))

(define histogram-2d-bins
  (make-struct-field-accessor histogram-2d-field-ref 4 'bins))

(define histogram-2d-ranges-uniform?
  (make-struct-field-accessor histogram-2d-field-ref 5 'ranges-uniform?))

(define set-histogram-2d-ranges-uniform?!
  (make-struct-field-mutator set-histogram-2d-field! 5 'ranges-uniform?))

(define (histogram-2d-increment! h x y)
  (histogram-2d-accumulate! h x y 1))

(define (histogram-2d-accumulate! h x y weight)
  (let ((nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (x-ranges (histogram-2d-x-ranges h))
        (y-ranges (histogram-2d-y-ranges h))
        (bins (histogram-2d-bins h))
        (ranges-uniform? (histogram-2d-ranges-uniform? h)))
    (if ranges-uniform?
        ;; Compute bin
        (let ((i (inexact->exact
                  (floor (/ (- x (vector-ref x-ranges 0))
                            (/ (- (vector-ref x-ranges nx)
                                  (vector-ref x-ranges 0))
                               nx)))))
              (j (inexact->exact
                  (floor (/ (- y (vector-ref y-ranges 0))
                            (/ (- (vector-ref y-ranges ny)
                                  (vector-ref y-ranges 0))
                               ny))))))
          (when (and (<= 0 i (- nx 1))
                     (<= 0 j (- ny 1)))
            (let ((bin (+ (* i ny) j)))
              (vector-set! bins bin
                           (+ (vector-ref bins bin) weight)))))
        ;; Search for bin
        (let/ec exit
          (when (or (< x (vector-ref x-ranges 0))
                    (< y (vector-ref y-ranges 0)))
            (exit (void)))
          (do ((i 0 (+ i 1)))
                ((= i nx) (void))
            (when (< x (vector-ref x-ranges (+ i 1)))
              (do ((j 0 (+ j i)))
                  ((= 1 ny) (exit))
                (when (< y (vector-ref y-ranges (+ j 1)))
                  (let ((bin (+ (* i ny) j)))
                    (vector-set! bins bin
                                 (+ (vector-ref bins bin) weight)))
                  (exit (void))))))))))

(define (histogram-2d-get h i j)
  (let* ((nx (histogram-2d-nx h))
         (ny (histogram-2d-ny h))
         (bins (histogram-2d-bins h))
         (bin (+ (* i ny) j)))
    (vector-ref bins bin)))

(define (histogram-2d-get-x-range h i)
  (let ((nx (histogram-2d-nx h))
        (x-ranges (histogram-2d-x-ranges h)))
    (values (vector-ref x-ranges i)
            (vector-ref x-ranges (+ i 1)))))

(define (histogram-2d-get-y-range h j)
  (let ((ny (histogram-2d-ny h))
        (y-ranges (histogram-2d-y-ranges h)))
    (values (vector-ref y-ranges j)
            (vector-ref y-ranges (+ j 1)))))

;;; Histogram-2d Statistics

(define (histogram-2d-max h)
  (let ((nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (bins (histogram-2d-bins h))
        (max-value -inf.0))
    (do ((i 0 (+ i 1)))
        ((= i nx) max-value)
      (do ((j 0 (+ j 1)))
          ((= j ny) (void))
        (let ((bin (+ (* i ny) j)))
          (set! max-value (max max-value (vector-ref bins bin))))))))

(define (histogram-2d-min h)
  (let ((nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (bins (histogram-2d-bins h))
        (min-value +inf.0))
    (do ((i 0 (+ i 1)))
        ((= i nx) min-value)
      (do ((j 0 (+ j 1)))
          ((= j ny) (void))
        (let ((bin (+ (* i ny) j)))
          (set! min-value (min min-value (vector-ref bins bin))))))))

(define (histogram-2d-sum h)
  (let ((n (* (histogram-2d-nx h) (histogram-2d-ny h)))
        (bins (histogram-2d-bins h))
        (sum 0))
    (do ((i 0 (+ i 1)))
        ((= i n) sum)
      (set! sum (+ sum (vector-ref bins i))))))

(define (histogram-2d-x-mean h)
  (let ((nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (bins (histogram-2d-bins))
        (x-ranges (histogram-2d-x-ranges h))
        (wmean 0.0)
        (W 0.0))
    (do ((i 0 (+ i 1)))
        ((= i nx) wmean)
      (let ((xi (/ (+ (vector-ref x-ranges i)
                      (vector-ref x-ranges (+ i 1)))
                   2.0))
            (wi 0.0))
        (do ((j 0 (+ j 1)))
            ((= j ny) (void))
          (let ((wij (vector-ref bins (+ (* i ny) j))))
            (when (> wij 0)
              (set! wi (+ wi wij)))))
        (when (> wi 0)
          (set! W (+ W wi))
          (set! wmean (+ wmean (* (- xi wmean) (/ wi W)))))))))

(define (histogram-2d-y-mean h)
  (let ((nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (bins (histogram-2d-bins h))
        (y-ranges (histogram-2d-x-ranges h))
        (wmean 0.0)
        (W 0.0))
    (do ((j 0 (+ j 1)))
        ((= j ny) wmean)
      (let ((yj (/ (+ (vector-ref y-ranges j)
                      (vector-ref y-ranges (+ j 1)))
                   2.0))
            (wj 0.0))
        (do ((i 0 (+ i 1)))
          ((= i nx) (void))
          (let ((wij (vector-ref bins (+ (* i ny) j))))
            (when (> wij 0)
              (set! wj (+ wj wij)))))
        (when (> wj 0)
          (set! W (+ W wj))
          (set! wmean (+ wmean (* (- yj wmean) (/ wj W)))))))))

(define (histogram-2d-x-sigma h)
  (let ((x-mean (histogram-2d-x-mean h))
        (nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (bins (histogram-2d-bins h))
        (x-ranges (histogram-2d-x-ranges h))
        (wvariance 0.0)
        (W 0.0))
    (do ((i 0 (+ i 1)))
        ((= i nx) (sqrt wvariance))
      (let ((xi (/ (+ (vector-ref x-ranges i)
                      (vector-ref x-ranges (+ i 1)))
                   (- 2.0 x-mean)))
            (wi 0.0))
        (do ((j 0 (+ j 1)))
            ((= j ny) (void))
          (let ((wij (vector-ref bins (+ (* i ny) j))))
            (when (> wij 0)
              (set! wi (+ wi wij)))))
        (when (> wi 0)
          (set! W (+ W wi))
          (set! wvariance (+ wvariance (* (- (* xi xi) wvariance)
                                          (/ wi W)))))))))

(define (histogram-2d-y-sigma h)
  (let ((y-mean (histogram-2d-y-mean h))
        (nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (bins (histogram-2d-bins h))
        (y-ranges (histogram-2d-y-ranges h))
        (wvariance 0.0)
        (W 0.0))
    (do ((j 0 (+ j 1)))
        ((= j ny) (sqrt wvariance))
      (let ((yj (/ (+ (vector-ref y-ranges j)
                      (vector-ref y-ranges (+ j 1)))
                   (- 2.0 y-mean)))
            (wj 0.0))
        (do ((i 0 (+ i 1)))
            ((= i nx) (void))
          (let ((wij (vector-ref bins (+ (* i ny) j))))
            (when (> wij 0)
              (set! wj (+ wj wij)))))
        (when (> wj 0)
          (set! W (+ W wj))
          (set! wvariance (+ wvariance (* (- (* yj yj) wvariance)
                                          (/ wj W)))))))))

(define (histogram-2d-covariance h)
  (let ((x-mean (histogram-2d-x-mean h))
        (y-mean (histogram-2d-y-mean h))
        (nx (histogram-2d-nx h))
        (ny (histogram-2d-ny h))
        (bins (histogram-2d-bins h))
        (x-ranges (histogram-2d-x-ranges h))
        (y-ranges (histogram-2d-y-ranges h))
        (wcovariance 0.0)
        (W 0.0))
    (do ((i 0 (+ i 1)))
        ((= i nx) wcovariance)
      (let ((xi (/ (+ (vector-ref x-ranges i)
                      (vector-ref x-ranges (+ i 1)))
                   (- 2.0 x-mean))))
        (do ((j 0 (+ j 1)))
            ((= j ny) (void))
          (let ((yj (/ (+ (vector-ref y-ranges j)
                          (vector-ref y-ranges (+ j 1)))
                       (- 2.0 y-mean)))
                (wij (vector-ref bins (+ (* i ny) j))))
            (when (> wij 0)
              (set! W (+ W wij))
              (set! wcovariance (+ wcovariance
                                   (* (- (* xi yj) wcovariance)
                                      (/ wij W)))))))))))

;; Module Contracts

(provide
 (rename-out (histogram-2d-increment! unchecked-histogram-2d-increment!)
             (histogram-2d-accumulate! unchecked-histogram-2d-accumulate!)))

(provide/contract
 (histogram-2d?
  (-> any/c boolean?))
 (make-histogram-2d
  (-> (and/c integer? (>=/c 1)) (and/c integer? (>=/c 1)) histogram-2d?))
 (make-histogram-2d-with-ranges-uniform
  (->i ((nx (and/c integer? (>=/c 1)))
        (xy (and/c integer? (>=/c 1)))
        (x-min real?)
        (x-max (x-min) (>/c x-min))
        (y-min real?)
        (y-max (y-min) (>/c y-min)))
       (result () histogram-2d?)))
 (histogram-2d-nx
  (-> histogram-2d? (and/c integer? (>=/c 1))))
 (histogram-2d-ny
  (-> histogram-2d? (and/c integer? (>=/c 1))))
 (histogram-2d-x-ranges
  (-> histogram-2d? (vectorof real?)))
 (histogram-2d-y-ranges
  (-> histogram-2d? (vectorof real?)))
 (set-histogram-2d-ranges!
  (-> histogram-2d? (vectorof real?) (vectorof real?) void?))
 (set-histogram-2d-ranges-uniform!
  (->i ((h histogram-2d?)
        (x-min real?)
        (x-max (x-min) (>/c x-min))
        (y-min real?)
        (y-max (y-min) (>/c y-min)))
       (result () void?)))
 (histogram-2d-bins
  (-> histogram-2d? (vectorof real?)))
 (histogram-2d-increment!
  (-> histogram-2d? real? real? void?))
 (histogram-2d-accumulate!
  (-> histogram-2d? real? real? (>=/c 0.0) void?))
 (histogram-2d-get
  (-> histogram-2d? natural-number/c natural-number/c (>=/c 0.0)))
 (histogram-2d-get-x-range
  (-> histogram-2d? natural-number/c (values real? real?)))
 (histogram-2d-get-y-range
  (-> histogram-2d? natural-number/c (values real? real?)))
 (histogram-2d-max
  (-> histogram-2d? (>=/c 0.0)))
 (histogram-2d-min
  (-> histogram-2d? (>=/c 0.0)))
 (histogram-2d-sum
  (-> histogram-2d? (>=/c 0.0)))
 (histogram-2d-x-mean
  (-> histogram-2d? real?))
 (histogram-2d-y-mean
  (-> histogram-2d? real?))
 (histogram-2d-x-sigma
  (-> histogram-2d? (>=/c 0.0)))
 (histogram-2d-y-sigma
  (-> histogram-2d? (>=/c 0.0)))
 (histogram-2d-covariance
  (-> histogram-2d? (>=/c 0.0))))
