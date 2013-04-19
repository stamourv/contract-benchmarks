#lang racket
;;; Science Collection
;;; special-functions/gamma.rkt
;;; Copyright (c) 2004-2010 M. Douglas Williams
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
;;; This is the module for the gamma, psi, and zeta special functions. They are
;;; provided as a single module to avoid circular module definitions.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/20/04  Marked as ready for Release 1.0.  Includes all of the
;;;                    gamma, psi, and zeta special functions for Release 1.0.
;;;                    Added contracts for functions. (MDW)
;;; 1.1.0    02/09/06  Added incomplete gamma functions. (MDW)
;;; 2.0.0    11/17/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme V4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

(require "../machine.rkt"
         "../math.rkt"
         "../chebyshev.rkt"
         "error.rkt"
         "exponential-integral.rkt"
         racket/include)

(include "gamma-imp.rkt")
(include "gamma-inc-imp.rkt")
(include "psi-imp.rkt")
(include "zeta-imp.rkt")

;;; Module Contracts

(provide
             ;; Gamma functons
 (rename-out (gamma unchecked-gamma)
             (lngamma unchecked-lngamma)
             (lngamma-sgn unchecked-lngamma-sgn)
             (gammainv unchecked-gammainv)
             (gamma* unchecked-gamma*)
             (gammastar unchecked-gammastar)
             (fact unchecked-fact)
             (lnfact unchecked-lnfact)
             (double-fact unchecked-double-fact)
             (lndouble-fact unchecked-lndouble-fact)
             (choose unchecked-choose)
             (lnchoose unchecked-lnchoose)
             ;; Incomplete Gamma functions
             (gamma-inc-Q unchecked-gamma-inc-Q)
             (gamma-inc-P unchecked-gamma-inc-P)
             (gamma-inc unchecked-gamma-inc)
             ;; Psi functions
             (psi-int unchecked-psi-int)
             (psi unchecked-psi)
             (psi-1piy unchecked-psi-1piy)
             (psi-1-int unchecked-psi-1-int)
             (psi-1 unchecked-psi-1)
             (psi-n unchecked-psi-n)
             ;; Zeta functions
             (zeta-int unchecked-zeta-int)
             (zeta unchecked-zeta)
             (zetam1-int unchecked-zetam1-int)
             (zetam1 unchecked-zetam1)
             (hzeta unchecked-hzeta)
             (eta-int unchecked-eta-int)
             (eta unchecked-eta)))

(provide/contract
 ;; Gamma functions
 (gamma
  (-> real? real?))
 (lngamma
  (-> real? real?))
 (lngamma-sgn
  (-> real? (values real? (integer-in -1 1))))
 (gammainv
  (-> real? real?))
 (gamma*
  (-> (>/c 0.0) real?))
 (gammastar
  (-> (>/c 0.0) real?))
 (fact
  (-> natural-number/c (>=/c 1.0)))
 (lnfact
  (-> natural-number/c (>=/c 0.0)))
 (double-fact
  (-> natural-number/c (>=/c 1.0)))
 (lndouble-fact
  (-> natural-number/c (>=/c 0.0)))
 (choose
  (-> natural-number/c natural-number/c (>=/c 1.0)))
 (lnchoose
  (-> natural-number/c natural-number/c (>=/c 0.0)))
 ;; Incomplete Gamma functions
 (gamma-inc-Q
  (-> (>/c 0.0) (>=/c 0.0) real?))
 (gamma-inc-P
  (-> (>/c 0.0) (>=/c 0.0) real?))
 (gamma-inc
  (-> real? (>=/c 0.0) real?))
 ;; Psi functions
 (psi-int
  (-> natural-number/c real?))
 (psi
  (-> real? real?))
 (psi-1piy
  (-> real? real?))
 (psi-1-int
  (-> natural-number/c real?))
 (psi-1
  (-> real? real?))
 (psi-n
  (-> natural-number/c real? real?))
 ;; Zeta Functions
 (zeta-int
  (-> integer? real?))
 (zeta
  (-> real? real?))
 (zetam1-int
  (-> integer? real?))
 (zetam1
  (-> real? real?))
 (hzeta
  (-> (>/c 1.0) (>/c 0.0) real?))
 (eta-int
  (-> integer? real?))
 (eta
  (-> real? real?)))
