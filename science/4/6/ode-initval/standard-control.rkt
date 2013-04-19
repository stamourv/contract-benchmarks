#lang racket
;;; Science Collection
;;; ode-initval/standard-control.rkt
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
;;; This module provides an ordinary differential equation solver
;;; capability for PLT Scheme.  
;;;
;;; Version  Date      Description
;;; 3.0.0    06/08/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Fixed header.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require scheme/unsafe/ops)

(require "control.rkt")

;(define-struct standard-control-state
;               (eps-abs
;                eps-rel
;                a_y
;                a_dydt))

(define-values (struct:standard-control-state
                standard-control-state-constructor
                standard-control-state?
                standard-control-state-field-ref
                set-standard-control-state-field!)
  (make-struct-type 'standard-control-state #f 4 0))

(define (make-standard-control-state)
  (standard-control-state-constructor 0.0 0.0 0.0 0.0))

(define standard-control-state-eps-abs
  (make-struct-field-accessor standard-control-state-field-ref 0 'eps-abs))
(define set-standard-control-state-eps-abs!
  (make-struct-field-mutator set-standard-control-state-field! 0 'eps-abs))

(define standard-control-state-eps-rel
  (make-struct-field-accessor standard-control-state-field-ref 1 'eps-rel))
(define set-standard-control-state-eps-rel!
  (make-struct-field-mutator set-standard-control-state-field! 1 'eps-rel))

(define standard-control-state-a_y
  (make-struct-field-accessor standard-control-state-field-ref 2 'a_y))
(define set-standard-control-state-a_y!
  (make-struct-field-mutator set-standard-control-state-field! 2 'a_y))

(define standard-control-state-a_dydt
  (make-struct-field-accessor standard-control-state-field-ref 3 'a_dydt))
(define set-standard-control-state-a_dydt!
  (make-struct-field-mutator set-standard-control-state-field! 3 'a_dydt))

(define (standard-control-init standard-control-state 
                               eps-abs eps-rel a_y a_dydt)
  (when (< eps-abs 0.0)
    (error 'standard-control-init "eps-abs is negative"))
  (when (< eps-rel 0.0)
    (error 'standard-control-init "eps-rel is negative"))
  (when (< a_y 0.0)
    (error 'standard-control-init "a_y is negative"))
  (when (< a_dydt 0.0)
    (error 'standard-control-init "a_dydt is negative"))
  (set-standard-control-state-eps-abs! standard-control-state eps-abs)
  (set-standard-control-state-eps-rel! standard-control-state eps-rel)
  (set-standard-control-state-a_y! standard-control-state a_y)
  (set-standard-control-state-a_dydt! standard-control-state a_dydt))

(define (standard-control-h-adjust state dim ord y y-err yp h)
  (let ((eps-abs (standard-control-state-eps-abs state))
        (eps-rel (standard-control-state-eps-rel state))
        (a_y (standard-control-state-a_y state))
        (a_dydt (standard-control-state-a_dydt state))
        (s 0.9)
        (h-old (unbox h))
        (rmax -inf.0))
    (for ((i (in-range dim)))
      (let* ((D0 (unsafe-fl+
                  (unsafe-fl* eps-rel
                              (unsafe-fl+
                               (unsafe-fl* a_y (abs (unsafe-vector-ref y i)))
                               (unsafe-fl* a_dydt (abs (unsafe-fl* h-old
                                                                   (unsafe-vector-ref yp i))))))
                    eps-abs))
             (r (unsafe-fl/ (abs (unsafe-vector-ref y-err i)) (abs D0))))
        (set! rmax (max r rmax))))
    (let/ec exit
      (cond ((unsafe-fl> rmax 1.1)
             ;; Decrease step, no more than a factor of 5, but a fraction
             ;; S more than scaling suggests (for better accuracy)
             (let ((r (unsafe-fl/ s (expt rmax (unsafe-fl/ 1.0 (exact->inexact ord))))))
               (when (unsafe-fl< r 0.2)
                 (set! r 0.2))
               (set-box! h (unsafe-fl* r h-old))
               (exit -1)))
            ((unsafe-fl< rmax 0.5)
             ;; Increase step, no more than factor of 5
             (let ((r (unsafe-fl/ s (expt rmax (unsafe-fl/ 1.0 (exact->inexact (unsafe-fx+ ord 1)))))))
               (when (unsafe-fl> r 5.0)
                 (set! r 5.0))
               (when (unsafe-fl< r 1.0) ; don't allow any decrease caused by s<1
                 (set! r 1.0))
               (set-box! h (unsafe-fl* r h-old))
               (exit 1)))
            (else
             0)))))

(define standard-control-type
  (make-ode-control-type
   "standard"
   make-standard-control-state
   standard-control-init
   standard-control-h-adjust))

(define (standard-control-new eps-abs eps-rel a_y a_dydt)
  (let ((control (make-ode-control standard-control-type)))
    (ode-control-init control eps-abs eps-rel a_y a_dydt)
    control))

(define (control-y-new eps-abs eps-rel)
  (standard-control-new eps-abs eps-rel 1.0 0.0))

(define (control-yp-new eps-abs eps-rel)
  (standard-control-new eps-abs eps-rel 0.0 1.0))

;;; Module Contracts

(provide (all-defined-out))
