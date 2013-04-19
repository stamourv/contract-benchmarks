#lang racket
;;; Science Collection
;;; ode-initval.rkt
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
;;; 2.0.0    11/19/07  Added header.  (Doug Williams)
;;; 2.0.1    01/29/08  Added contracts.  (Doug Williams)
;;; 2.1.0    06/07/08  Changed the components to be separate modules
;;;                    with this one providing the contracts.  Made
;;;                    changes required for V4.0.  (Doug Williams)
;;; 2.1.1    06/14/08  Add unchecked procedures.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "ode-initval/system.rkt"
         "ode-initval/step.rkt"
         "ode-initval/control.rkt"
         "ode-initval/standard-control.rkt"
         "ode-initval/evolve.rkt")

(provide
 (rename-out (ode-system-function-eval unchecked-ode-system-function-eval)
             (ode-system-jacobian-eval unchecked-ode-system-jacobian-eval)
             (ode-step-apply unchecked-ode-step-apply)
             (ode-step-reset unchecked-ode-step-reset)
             (ode-evolve-apply unchecked-ode-evolve-apply)
             (ode-evolve-reset unchecked-ode-evolve-reset)))

;;; Contracts

(provide/contract
 (ode-system?
  (-> any/c boolean?))
 (make-ode-system
  (-> procedure? (or/c procedure? false/c) natural-number/c list? ode-system?))
 (ode-system-function
  (-> ode-system? procedure?))
 (ode-system-jacobian
  (-> ode-system? (or/c procedure? false/c)))
 (ode-system-dimension
  (-> ode-system? natural-number/c))
 (ode-system-params
  (-> ode-system? list?))
 (ode-system-function-eval
  (-> ode-system? inexact-real? (vectorof inexact-real?) (vectorof inexact-real?) void?))
 (ode-system-jacobian-eval
  (-> ode-system? inexact-real? (vectorof inexact-real?) (vectorof inexact-real?) (vectorof inexact-real?) void?))
 (ode-step-type?
  (-> any/c boolean?))
 (make-ode-step-type
  (-> string? boolean? boolean? procedure? procedure? procedure? procedure? ode-step-type?))
 (ode-step-type-name
  (-> ode-step-type? string?))
 (ode-step-type-can-use-dydt-in?
  (-> ode-step-type? boolean?))
 (ode-step-type-gives-exact-dydt-out?
  (-> ode-step-type? boolean?))
 (ode-step-type-make
  (-> ode-step-type? procedure?))
 (ode-step-type-apply
  (-> ode-step-type? procedure?))
 (ode-step-type-reset
  (-> ode-step-type? procedure?))
 (ode-step-type-order
  (-> ode-step-type? procedure?))
 (ode-step?
  (-> any/c boolean?))
 (make-ode-step
  (-> ode-step-type? natural-number/c ode-step?))
 (ode-step-step-type
  (-> ode-step? ode-step-type?))
 (ode-step-dimension
  (-> ode-step? natural-number/c))
 (ode-step-state
  (-> ode-step? any))
 (ode-step-name
  (-> ode-step? string?))
 (ode-step-order
  (-> ode-step? natural-number/c))
 (ode-step-apply
  (-> ode-step? inexact-real? inexact-real? (vectorof inexact-real?) (vectorof inexact-real?)
      (vectorof inexact-real?) (vectorof inexact-real?) ode-system? any))
 (ode-step-reset
  (-> ode-step? void))
 (standard-control-state?
  (-> any/c boolean?))
 (make-standard-control-state
  (-> standard-control-state?))
 (standard-control-state-eps-abs
  (-> standard-control-state? inexact-real?))
 (standard-control-state-eps-rel
  (-> standard-control-state? inexact-real?))
 (standard-control-state-a_y
  (-> standard-control-state? inexact-real?))
 (standard-control-state-a_dydt
  (-> standard-control-state? inexact-real?))
 )

(provide
 make-ode-control
 ode-control-init
 ode-control-name
 ode-control-h-adjust
 make-ode-evolve
 ode-evolve-count
 ode-evolve-failed-steps
 standard-control-new
 control-y-new
 control-yp-new
 ode-evolve-reset
 (contract-out [ode-evolve-apply
                (-> ode-evolve?
                    ode-control?
                    ode-step?
                    ode-system?
                    box?
                    real?
                    box?
                    (vectorof real?)
                    any)]))

;;; Routines
;;; Include the predefined ODE solvers.

(include "ode-initval/rk2.rkt")
(include "ode-initval/rk4.rkt")
(include "ode-initval/rkf45.rkt")

(provide 
 rk2-ode-type
 rk4-ode-type
 rkf45-ode-type)
