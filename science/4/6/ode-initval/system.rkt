#lang racket
;;; Science Collection
;;; ode-initval/ode-initval/system.rkt
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
;;; This module provides all of the random distributions for the PLT
;;; Science Collection as a single module.
;;;
;;; Version  Date      Description
;;; 2.1.0    06/06/06  Creating separate modules and making V4.0 changes.
;;;                    (Doug Williams)
;;; 3.0.0    06/08/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Fixed header.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

;;; Description of a systems of ODEs.
;;;
;;; y' = f(t, y) = dydt(t, y)
;;;
;;; The system is specified by giving the right-hand-side of the
;;; equation and possibly a Jacobian function.
;;;
;;; Some methods require the Jacobian function, which calculated the
;;; matrix dfdy and the vector dfdt.

(define-struct ode-system
               (function
                jacobian
                dimension
                params))

;; ode-system-function-eval:
;;   ode-system x real x vector of real x vector of real -> void
(define (ode-system-function-eval ode-system t y dydt)
  ((ode-system-function ode-system)
   t y dydt (ode-system-params ode-system)))

;; ode-system-jacobian-eval:
;;   ode-system x real x vector of real x matrix of real x vector of real -> void
(define (ode-system-jacobian-eval ode-system  t y dfdy dfdt)
  ((ode-system-jacobian ode-system)
   t y dfdy dfdt (ode-system-params ode-system)))

;;; Module Contracts

(provide (all-defined-out))
