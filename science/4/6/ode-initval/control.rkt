#lang racket
;;; Science Collection
;;; ode-initval/control.rkt
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

(require "step.rkt")

(define-struct ode-control-type
               (name
                make
                init
                h-adjust))

;(define-struct ode-control
;               (control-type
;                state))
(define-values (struct:ode-control
                ode-control-constructor
                ode-control?
                ode-control-field-ref
                set-ode-control-field!)
  (make-struct-type 'ode-control #f 2 0))

(define (make-ode-control ode-control-type)
  (ode-control-constructor
   ode-control-type
   ((ode-control-type-make ode-control-type))))

(define ode-control-control-type
  (make-struct-field-accessor ode-control-field-ref 0 'control-type))
(define set-ode-control-control-type!
  (make-struct-field-mutator set-ode-control-field! 0 'control-type))

(define ode-control-state
  (make-struct-field-accessor ode-control-field-ref 1 'state))
(define set-ode-control-state!
  (make-struct-field-mutator set-ode-control-field! 1 'state))

(define (ode-control-init control eps-abs eps-rel a_y a_dydt)
  ((ode-control-type-init (ode-control-control-type control))
   (ode-control-state control) eps-abs eps-rel a_y a_dydt))

(define (ode-control-name control)
  (ode-control-type-name (ode-control-control-type control)))

(define (ode-control-h-adjust control step y0 y-err dydt h)
  ((ode-control-type-h-adjust (ode-control-control-type control))
   (ode-control-state control) (ode-step-dimension step)
   ((ode-step-type-order (ode-step-step-type step))
    (ode-step-state step))
   y0 y-err dydt h))

;;; Module Contracts

(provide (all-defined-out))
