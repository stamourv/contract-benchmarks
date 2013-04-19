#lang racket/base
;;; Racket Science Collection
;;; machine.rkt
;;; Copyright (c) 2004-2011 M. Douglas Williams
;;;
;;; This file is part of the Racket Science Collection.
;;;
;;; The Racket Science Collection is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the License or
;;; (at your option) any later version.
;;;
;;; The Racket Science Collection is distributed in the hope that it will be
;;; useful, but WITHOUT WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with the Racket Science Collection.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;; -----------------------------------------------------------------------------
;;;
;;; This code is based on the machine precision and limits constants in the GNU
;;; Scientific Library (GSL), which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 0.1.0    08/14/04  This is the initial release of the machine constants
;;;                    ported from GSL. (MDW)
;;; 1.0.0    09/20/04  Marked as ready for Release 1.0 (MDW)
;;; 3.0.0    06/09/08  Changes required for PLT Scheme V4.0.  (MDW)
;;; 4.0.0    05/12/10  Changed the header and restructured the code. (MDW)
;;; 4.0.1    04/22/11  Removed unused constants. (MDW)

;;; Magic constants; mostly for the benefit of the implementation.

(define double-epsilon        2.2204460492503131e-16)
(define sqrt-double-epsilon   1.4901161193847656e-08)
(define root3-double-epsilon  6.0554544523933429e-06)
(define root4-double-epsilon  1.2207031250000000e-04)
(define root5-double-epsilon  7.4009597974140505e-04)
(define root6-double-epsilon  2.4607833005759251e-03)
(define log-double-epsilon   -3.6043653389117154e+01)

(define double-min        2.2250738585072014e-308)
(define sqrt-double-min   1.4916681462400413e-154)
(define root3-double-min  2.8126442852362996e-103)
(define root4-double-min  1.2213386697554620e-77)
(define root5-double-min  2.9476022969691763e-62)
(define root6-double-min  5.3034368905798218e-52)
(define log-double-min   -7.0839641853226408e+02)

(define double-max        1.7976931348623157e+308)
(define sqrt-double-max   1.3407807929942596e+154)
(define root3-double-max  5.6438030941222897e+102)
(define root4-double-max  1.1579208923731620e+77)
(define root5-double-max  4.4765466227572707e+61)
(define root6-double-max  2.3756689782295612e+51)
(define log-double-max    7.0978271289338397e+02)

;;; A little internal backwards compatibility.
;(define machine-eps double-epsilon)

;;; Here are constants related to or derived from
;;; machine constants. These are not to be confused with
;;; the constants that define various precision levels
;;; for the precision/error system.
;;;
;;; This information is determined at configure time
;;; and is platform dependent. Edit at your own risk.

;;; machine precision constants
;;; (define machine-eps 1.0e-15)
;(define sqrt-machine-eps   3.2e-08)
;(define root3-machine-eps  1.0e-05)
;(define root4-machine-eps  0.000178)
;(define root5-machine-eps  0.00100)
;(define root6-machine-eps  0.00316)
;(define log-machine-eps   -34.54)

;;; Module Contracts

(provide (all-defined-out))
