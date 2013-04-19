#lang racket
;;; Science Collection
;;; random-source.rkt
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
;;; This code adds some additional functionality to the PLT Scheme implementation
;;; of SRFI 27 provided with PLT Scheme V207 (and presumably later versions).
;;;
;;; The main additional functionality is to define a parameter,
;;; current-random-source, that provides a separate random stream reference for
;;; each thread.  The default value for this random stream reference is
;;; default-random-stream as provided by SRFI 27. A guard procedure ensures that
;;; the value of current-random-source is indeed a random-source, otherwise a
;;; type error is raised.
;;;
;;; As of V371.1, there is a new implementation of SRFI 27 in PLT Scheme.  The
;;; underlying PLT Scheme random source rountines were modified at some point to
;;; use the same algorithms as SRFI 27. The new SRFI 27 implemtation wrappers
;;; around this functionality.  There are a few differences between the old
;;; implementation and the new one that required changes in this module.  In
;;; particular, some of the SRFI 27 procedures are now macros. [This has been
;;; reverted in PLT Scheme.]
;;;
;;; The following is OBE.
;;; Instead of the set-random-source-state! procedure just being an alias for
;;; random-source-state-set!, it now calls it directly. This was done because
;;; the latter is now a macro and the aliasing does not work.  However, this also
;;; breaks the ability to set the state of the default-random-source.
;;;
;;; 
;;;
;;; -----------------------------------------------------------------------------
;;;
;;; Version  Date      Description
;;; 0.9.0    08/05/04  This is the initial release of the random source module to
;;;                    augment SRFI 27. (MDW)
;;; 1.0.0    09/20/04  Marked as ready for Release 1.0. (MDW)
;;; 1.0.1    07/13/05  Added make-random-source-vector. (MDW)
;;; 1.0.2    10/18/05  Added optional second argument to
;;;                    make-random-source-vector. (MDW)
;;; 1.0.3    08/24/07  Updated to be compatible with the new SRFI 27
;;;                    implementation. (MDW)
;;; 1.0.4    09/12/07  The SRFI 27 implementation is changing  back to the same
;;;                    interface as before, i.e., no macros for the standard
;;;                    functionality. (MDW)
;;; 2.0.0    11/17/07  Added unchecked version of functions and getting ready for
;;;                    PLT Scheme V4.0. (MDW)
;;; 2.1.0    06/07/08  More PLT Scheme V4.0 changes. (MDW)
;;; 4.0.0    05/12/10  Changed the header and restructured the code. (MDW)

(require srfi/27)

;;; Provide a parameter for the current random source - See PLT
;;; MzScheme: Language Manual, Section 7.7 Parameters.

(define current-random-source
  (make-parameter
   default-random-source
   (lambda (x)
     (when (not (random-source? x))
       (raise-type-error 'current-random-source
                         "random-source" x))
     x)))

;;; The macros with-random-source and with-new-random-source provide
;;; a convenient method for executing a body of code with a given
;;; random stream.  The body is executed with current-random-source
;;; set to the specified random-source.

(define-syntax-rule (with-random-source random-source
                      body ...)
  (parameterize ((current-random-source random-source))
    body ...))

(define-syntax-rule (with-new-random-source
                      body ...)
  (parameterize ((current-random-source
                  (make-random-source)))
    body ...))

;;; (random-uniform-int r n) -> exact-nonnegative-integer?
;;;   r : random-source?
;;;   n : exact-positive-integer?
;;; (random-uniform-int n) -> exact-nonnegative-integer?
;;;   n : exact-positive-integer?
;;; The procedure random-uniform-int returns an integer in the range
;;; 0 ... n-1 using the specified random-source or (current-random-
;;; source) is none is specified.  Note that the random-integer and
;;; random-real functions from SRFI 27 DO NOT understand (current-
;;; random-source) and always use default random-source.
(define random-uniform-int
  (case-lambda
    ((r n)
     ;; Note that random-source-make-integers returns a procedure
     ;; that must be applied to get the random integer.  Thus the
     ;; extra set of parentheses.
     ((random-source-make-integers r) n))
    ((n)
     (random-uniform-int (current-random-source) n))))

;;; (random-uniform r) -> (and/c inexact-real? (real-in 0.0 1.0))
;;;   r : random-source?
;;; (random-uniform) -> (and/c inexact-real? (real-in 0.0 1.0))
;;; The procedure random-uniform returns a double precision real in
;;; the range (0.0, 1.0) (non-inclusive) using the specified
;;; random-source or (current-random-source) if none is specified.
;;; Note that the random-integer and random-real functions from SRFI
;;; 27 DO NOT understand (current-random-source) and always use 
;;; default-random-source.
(define random-uniform
  (case-lambda
    ((r)
     ;; Note that random-source-make-reals returns a procedure that
     ;; must be applied to get the random number. Thus the extra
     ;; set of parentheses.
     ((random-source-make-reals r)))
    (()
     (random-uniform (current-random-source)))))

;;; Also provide alternatives to random-source-state-ref and
;;; random-source-state-set! from SRFI 27.
(define random-source-state random-source-state-ref)
(define set-random-source-state! random-source-state-set!)

;The following was needed at a point during the V371 timeframe when the
;SRFI 27 implementation was in flux.  Basically, random-source-state-set!
; was implemented as a macro and was not a first-class object.
;(define (set-random-source-state! s state)
;  (random-source-state-set! s state))

;;; (make-random-source-vector n i) -> (vectorof random-source)
;;;   n : exact-nonnegative-integer?
;;;   i : exact-non-negative-integer?
;;; (make-random-source-vector n) -> (vectorof random-source)
;;;   n : exact-nonnegative-integer?
(define make-random-source-vector
  (case-lambda
    ((n i)
     (build-vector
      n
      (lambda (j)
        (let ((random-stream (make-random-source)))
          (random-source-pseudo-randomize! random-stream i j)
          random-stream))))
    ((n)
     (build-vector
      n
      (lambda (j)
        (let ((random-stream (make-random-source)))
          (random-source-pseudo-randomize! random-stream 0 j)
          random-stream))))))

;;; Module Contracts

(provide
 (all-from-out srfi/27)
 current-random-source
 with-random-source
 with-new-random-source
 (rename-out (random-uniform-int unchecked-random-uniform-int)
             (random-uniform unchecked-random-uniform)
             (random-source-state unchecked-random-source-state)
             (set-random-source-state! unchecked-set-random-source-state!)
             (make-random-source-vector unchecked-make-random-source-vector)))

(provide/contract
 (random-uniform-int
  (case-> (-> random-source? exact-positive-integer? exact-nonnegative-integer?)
          (-> exact-positive-integer? exact-nonnegative-integer?)))
 (random-uniform
  (case-> (-> random-source? (and/c inexact-real? (real-in 0.0 1.0)))
          (-> (and/c inexact-real? (real-in 0.0 1.0)))))
 (random-source-state
  (-> random-source? any))
 (set-random-source-state!
  (-> random-source? any/c void?))
 (make-random-source-vector
  (case-> (-> exact-nonnegative-integer? exact-nonnegative-integer?
              (vectorof random-source?))
          (-> exact-nonnegative-integer? (vectorof random-source?)))))
