#lang racket/base
;;; Science Collection
;;; constants.rkt
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
;;; This code is based on the physical constants in the GNU Scientific Library
;;; (gsl), which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 4.0.0    05/22/10  Initial release of the physical constants. (MDW)

(require "constants/num-constants.rkt"
         "constants/cgs-constants.rkt"
         "constants/cgsm-constants.rkt"
         "constants/mks-constants.rkt"
         "constants/mksa-constants.rkt")

;;; Module Contracts

(provide (all-from-out "constants/num-constants.rkt")
         (all-from-out "constants/cgs-constants.rkt")
         (all-from-out "constants/cgsm-constants.rkt")
         (all-from-out "constants/mks-constants.rkt")
         (all-from-out "constants/mksa-constants.rkt"))
