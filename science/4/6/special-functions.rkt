#lang racket
;;; Science Collection
;;; special-functions.rkt
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
;;; -----------------------------------------------------------------------------
;;;
;;; This is the sub-collection for the special functions provided by the Science
;;; Collection. These include:
;;;   - Error Functions
;;;   - Gamma Functions
;;;   - Psi (Digamma) Functions
;;;   - Zeta Functions
;;;   - Beta Functions
;;;   - Exponential Integral Functions
;;; Note that the gamma, psi, and zeta routines are provided as a single module.
;;; This is because their definition are interdependent and would result in
;;; circular modules. Individual modules are available (as gamma.ss, psi.ss, and
;;; zeta.ss in the special-functions sub-collection), but they all load the
;;; combined file and provide just the corresponding routines from it.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/20/04  Marked as ready for Release 1.0. Includes all of the
;;;                    special-functions for Release 1.0. (MDW)
;;; 1.1.0    02/09/06  Added beta and exponential integral functions. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

(require "special-functions/error.rkt"
         "special-functions/gamma.rkt"
         "special-functions/beta.rkt"
         "special-functions/exponential-integral.rkt")

(provide
 (all-from-out "special-functions/error.rkt")
 (all-from-out "special-functions/gamma.rkt")
 (all-from-out "special-functions/beta.rkt")
 (all-from-out "special-functions/exponential-integral.rkt"))
