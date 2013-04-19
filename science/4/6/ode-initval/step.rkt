#lang racket
;;; Science Collection
;;; ode-initval/step.rkt
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
;;; Version  Date      Description
;;; 3.0.0    06/08/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Fixed header.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(define-struct ode-step-type
               (name
                can-use-dydt-in?
                gives-exact-dydt-out?
                make
                apply
                reset
                order))

;(define-struct ode-step
;               (type
;                dimension
;                state))
(define-values (struct:ode-step
                ode-step-constructor
                ode-step?
                ode-step-field-ref
                set-ode-step-field!)
  (make-struct-type 'ode-step #f 3 0))

(define (make-ode-step step-type dim)
  (ode-step-constructor
   step-type
   dim
   ((ode-step-type-make step-type) dim)))

(define ode-step-step-type
  (make-struct-field-accessor ode-step-field-ref 0 'step-type))
(define set-ode-stepper-step-type!
  (make-struct-field-mutator set-ode-step-field! 0 'step-type))

(define ode-step-dimension
  (make-struct-field-accessor ode-step-field-ref 1 'dimension))
(define set-ode-step-dimension!
  (make-struct-field-mutator set-ode-step-field! 1 'dimension))

(define ode-step-state
  (make-struct-field-accessor ode-step-field-ref 2 'state))
(define set-ode-step-state!
  (make-struct-field-mutator set-ode-step-field! 2 'state))

;; ode-step-name: ode-step -> string
(define (ode-step-name step)
  (ode-step-type-name (ode-step-step-type step)))

;; ode-step-order: ode-step -> integer
(define (ode-step-order step)
  ((ode-step-type-order (ode-step-step-type step))
   (ode-step-state step)))

;; ode-step-apply:
;;   ode-step x real x real x vector of real x vector of real x
;;   vector of real x vector of real x ode-system  -> void
(define (ode-step-apply step t h y y-err dydt-in dydt-out dydt)
  ((ode-step-type-apply (ode-step-step-type step))
   (ode-step-state step) (ode-step-dimension step)
   t h y y-err dydt-in dydt-out dydt))

;; ode-step-reset: ode-step -> void
(define (ode-step-reset step)
  ((ode-step-type-reset (ode-step-step-type step))
   (ode-step-state step) (ode-step-dimension step)))

;;; Module Contracts

(provide (all-defined-out))
