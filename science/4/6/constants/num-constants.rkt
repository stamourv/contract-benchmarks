#lang racket/base
;;; Racket Science Collection
;;; num-constants.rkt
;;; Copyright (c) 2010-2011 M. Douglas Williams
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
;;; This code is based on the numeric constants in the GNU Scientific Library
;;; (GSL), which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 4.0.0    05/22/10  Initial release of the numeric constants. (MDW)

(define num-fine-structure 7.297352533e-3) ; 1
(define num-avogadro 6.02214199e23) ; 1 / mol
(define num-yotta 1e24) ; 1
(define num-zetta 1e21) ; 1
(define num-exa 1e18) ; 1
(define num-peta 1e15) ; 1
(define num-tera 1e12) ; 1
(define num-giga 1e9) ; 1
(define num-mega 1e6) ; 1
(define num-kilo 1e3) ; 1
(define num-milli 1e-3) ; 1
(define num-micro 1e-6) ; 1
(define num-nano 1e-9) ; 1
(define num-pico 1e-12) ; 1
(define num-femto 1e-15) ; 1
(define num-atto 1e-18) ; 1
(define num-zepto 1e-21) ; 1
(define num-yacto 1e-24) ; 1

;;; Module Contracts

(provide (all-defined-out))
