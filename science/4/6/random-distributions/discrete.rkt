#lang racket
;;; Science Collection
;;; random-distributions/discrete.rkt
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
;;; This module implements general discrete distribution.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Marked as ready for Release 1.0.  Added 
;;;                    contracts for functions.  (Doug Williams)
;;; 1.1.0    04/18/06  Made random-discrete use a binary search.
;;;                    (Doug Williams)
;;; 1.1.1    04/20/06  Changed random-discrete to use Walker's O(1)
;;;                    algorithm.  (Doug Williams)
;;; 2.0.0    11/19/07  Added unchecked versions of functions and
;;;                    getting ready for PLT Scheme 4.0 (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changed (when (not ...) ...) to
;;;                    (unless ... ...).  Doug Williams
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Added discrete-n to contracts. (MDW)

;;; Data Definition

;;; discrete structure
;;;
;;; An instance of the discrete structure, created by make-discrete,
;;; represents a general discrete distribution.  Only the discrete?
;;; function is exported.
(define-values (struct:discrete
                discrete-constructor
                discrete?
                discrete-field-ref
                set-discrete-field!)
  (make-struct-type 'discrete #f 5 0))

(require "../random-source.rkt")

(define discrete-n
  (make-struct-field-accessor discrete-field-ref 0 'n))

(define discrete-a
  (make-struct-field-accessor discrete-field-ref 1 'a))

(define discrete-f
  (make-struct-field-accessor discrete-field-ref 2 'f))

(define discrete-p
  (make-struct-field-accessor discrete-field-ref 3 'p))

(define discrete-c
  (make-struct-field-accessor discrete-field-ref 4 'c))

;;; make-discrete: vector -> discrete
;;; This function accepts a vector of weights and returns a discrete
;;; probability structure that can be passed to random-discrete to
;;; generate random variates.  The weights do not have to sum to 1.0.
(define (make-discrete w)
  (let* ((n (vector-length w))
         (a (make-vector n))
         (f (make-vector n))
         (sum 0.0)
         (cumm 0.0)
         (mean (/ 1.0 n))
         (smalls '())
         (bigs '())
         (e (make-vector n))
         (p (make-vector n))
         (c (make-vector n)))
    ;; find sum
    (do ((i 0 (+ i 1)))
        ((= i n) (void))
      (let ((wi (vector-ref w i)))
        (set! sum (+ sum wi))))
    ;; normalize weights and partition into bigs and smalls
    ;; also compute pdf and cdf values
    (do ((i 0 (+ i 1)))
        ((= i n) (void))
      (let* ((wi (vector-ref w i))
             (q (/ wi sum)))
        ;; normalize
        (vector-set! e i q)
        ;; compute pdf
        (vector-set! p i q)
        ;; compute cdf
        (set! cumm (+ cumm q))
        (vector-set! c i cumm)
        ;; partition
        (if (< q mean)
            (set! smalls (cons i smalls))
            (set! bigs (cons i bigs)))))
    ;; work through the smalls
    (let loop ()
      (unless (null? smalls)
        (let ((s (car smalls)))
          (set! smalls (cdr smalls))
          (if (null? bigs)
              (begin
                (vector-set! a s s)
                (vector-set! f s 1.0)
                (loop))
              (let ((b (car bigs)))
                (set! bigs (cdr bigs))
                (vector-set! a s b)
                (vector-set! f s (* n (vector-ref e s)))
                (let ((d (- mean (vector-ref e s))))
                  (vector-set! e s (+ (vector-ref e s) d))
                  (vector-set! e b (- (vector-ref e b) d)))
                (cond ((< (vector-ref e b) mean)
                       (set! smalls (cons b smalls)))
                      ((> (vector-ref e b) mean)
                       (set! bigs (cons b bigs)))
                      (else
                       (vector-set! a b b)
                       (vector-set! f b 1.0))))))
        (loop)))
    ;; work through remaining bigs
    (let loop ()
      (unless (null? bigs)
        (let ((b (car bigs)))
          (set! bigs (cdr bigs))
          (vector-set! a b b)
          (vector-set! f b 1.0))
        (loop)))
    ;; apply Knuth's convention
    (do ((i 0 (+ i 1)))
        ((= i n) (void))
      (vector-set! f i (/ (+ (vector-ref f i) i) n)))
    ;; return the discrete
    (discrete-constructor n a f p c)))

;;; random-discrete: random-source x discrete -> integer
;;; random-discrete: discrete -> integer
;;; This function returns a random variate from the given discrete
;;; distribution given by d.
(define random-discrete
  (case-lambda
    ((r d)
     (let* ((u (unchecked-random-uniform r))
            (c (inexact->exact (floor (* u (discrete-n d)))))
            (f (vector-ref (discrete-f d) c)))
       (if (= f 1.0)
           c
           (if (< u f)
               c
               (vector-ref (discrete-a d) c)))))
    ((d)
     (random-discrete (current-random-source) d))))

;;; discrete-pdf: discrete x integer -> real
;;; This function computes the probability density p(k) at k for a
;;; discrete distribution given by d.
(define (discrete-pdf d k)
  (let* ((p (discrete-p d))
         (n (vector-length p)))
    (if (or (< k 0)
            (>= k n))
        0.0
        (vector-ref p k))))

;;; discrete-cdf: discrete x integer -> real
;;; This function computes the cummulative density d(k) at k for a
;;; discrete distribution given by d.
(define (discrete-cdf d k)
  (let* ((c (discrete-c d))
         (n (vector-length c)))
    (cond ((< k 0)
           0.0)
          ((> k n)
           1.0)
          (else
           (vector-ref c k)))))

;; Module Contracts

(provide
 (rename-out (random-discrete unchecked-random-discrete)
             (discrete-pdf unchecked-discrete-pdf)
             (discrete-cdf unchecked-discrete-cdf)))

(provide/contract
 (discrete?
  (-> any/c boolean?))
 (make-discrete
  (-> (vectorof (>=/c 0.0)) discrete?))
 (discrete-n
  (-> discrete? exact-nonnegative-integer?))
 (random-discrete
  (case-> (-> random-source? discrete? natural-number/c)
          (-> discrete? natural-number/c)))
 (discrete-pdf
  (-> discrete? integer? (real-in 0.0 1.0)))
 (discrete-cdf
  (-> discrete? integer? (real-in 0.0 1.0))))
