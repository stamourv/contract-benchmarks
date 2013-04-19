#lang racket
;;; Science Collection
;;; ode-initval/evolve.rkt
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

(require "system.rkt"
         "step.rkt"
         "control.rkt")

;(define-struct ode-evolve
;               (dimension
;                y0
;                y-err
;                dydt-in
;                dydt-out
;                last-step
;                count
;                failed-steps))
(define-values (struct:ode-evolve
                ode-evolve-constructor
                ode-evolve?
                ode-evolve-field-ref
                set-ode-evolve-field!)
  (make-struct-type 'ode-evolve #f 8 0))

(define (make-ode-evolve dim)
  (ode-evolve-constructor
   dim
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   (make-vector dim 0.0)
   0.0 0 0))

(define ode-evolve-dimension
  (make-struct-field-accessor ode-evolve-field-ref 0 'dimension))
(define set-ode-evolve-dimension!
  (make-struct-field-mutator set-ode-evolve-field! 0 'dimension))

(define ode-evolve-y0
  (make-struct-field-accessor ode-evolve-field-ref 1 'y0))
(define set-ode-evolve-y0!
  (make-struct-field-mutator set-ode-evolve-field! 1 'y0))

(define ode-evolve-y-err
  (make-struct-field-accessor ode-evolve-field-ref 2 'y-err))
(define set-ode-evolve-y-err!
  (make-struct-field-mutator set-ode-evolve-field! 2 'y-err))

(define ode-evolve-dydt-in
  (make-struct-field-accessor ode-evolve-field-ref 3 'dydt-in))
(define set-ode-evolve-dydt-in!
  (make-struct-field-mutator set-ode-evolve-field! 3 'dydt-in))

(define ode-evolve-dydt-out
  (make-struct-field-accessor ode-evolve-field-ref 4 'dydt-out))
(define set-ode-evolve-dydt-out!
  (make-struct-field-mutator set-ode-evolve-field! 4 'dydt-out))

(define ode-evolve-last-step
  (make-struct-field-accessor ode-evolve-field-ref 5 'last-step))
(define set-ode-evolve-last-step!
  (make-struct-field-mutator set-ode-evolve-field! 5 'last-step))

(define ode-evolve-count
  (make-struct-field-accessor ode-evolve-field-ref 6 'count))
(define set-ode-evolve-count!
  (make-struct-field-mutator set-ode-evolve-field! 6 'count))

(define ode-evolve-failed-steps
  (make-struct-field-accessor ode-evolve-field-ref 7 'failed-steps))
(define set-ode-evolve-failed-steps!
  (make-struct-field-mutator set-ode-evolve-field! 7 'failed-steps))

(define (ode-evolve-reset evolve)
  (set-ode-evolve-count! evolve 0)
  (set-ode-evolve-failed-steps! evolve 0)
  (set-ode-evolve-last-step! evolve 0.0))

(define (ode-evolve-apply evolve control step dydt
                          t t1 h y)
  (let* ((t0 (unbox t))
         (h0 (unbox h))
         (final-step #f)
         (dt (unsafe-fl- t1 t0)))
    (unless (unsafe-fx= (ode-evolve-dimension evolve)
               (ode-step-dimension step))
      (error 'ode-evolve-apply
             "step dimension must match evolution dimension"))
    (when (or (and (unsafe-fl< dt 0.0) (unsafe-fl> h0 0.0))
              (and (unsafe-fl> dt 0.0) (unsafe-fl< h0 0.0)))
      (error 'ode-evolve-apply
             "step direction must match interval direction"))
    ;; No need to copy if we can't control the step size
    (when control
      (vector-copy! (ode-evolve-y0 evolve) 0 y))
    ;; Calculate initial dydt once if the method can benefit
    (when (ode-step-type-can-use-dydt-in?
           (ode-step-step-type step))
      (ode-system-function-eval
       dydt t0 y (ode-evolve-dydt-in evolve)))
    (let try-step ()
      (if (or (and (unsafe-fl>= dt 0.0) (unsafe-fl> h0 dt))
              (and (unsafe-fl< dt 0.0) (unsafe-fl< h0 dt)))
          (begin
            (set! h0 dt)
            (set! final-step #t))
          (set! final-step #f))
      (if (ode-step-type-can-use-dydt-in?
           (ode-step-step-type step))
          (ode-step-apply
           step
           t0 h0 y (ode-evolve-y-err evolve)
           (ode-evolve-dydt-in evolve) (ode-evolve-dydt-out evolve)
           dydt)
          (ode-step-apply
           step
           t0 h0 y (ode-evolve-y-err evolve)
           #f (ode-evolve-dydt-out evolve)
           dydt))
      (set-ode-evolve-count! evolve (unsafe-fx+ (ode-evolve-count evolve) 1))
      (set-ode-evolve-last-step! evolve h0)
      (if final-step
          (set-box! t t1)
          (set-box! t (unsafe-fl+ t0 h0)))
      (when control
        (let* ((h0-boxed (box h0))
               (h-adjust-status
                (ode-control-h-adjust
                 control step y (ode-evolve-y-err evolve)
                 (ode-evolve-dydt-out evolve) h0-boxed)))
          (set! h0 (unbox h0-boxed))
          (when (= h-adjust-status -1)
            (vector-copy! y 0 (ode-evolve-y0 evolve))
            (set-ode-evolve-failed-steps! evolve
                                          (unsafe-fx+ (ode-evolve-failed-steps evolve) 1))
            (try-step)))))
    (set-box! h h0)))

;;; Module Contracts

(provide (all-defined-out))
