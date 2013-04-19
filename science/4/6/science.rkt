#lang racket/base
;;; Science Collection
;;; science.rkt
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
;;; This is the top-level module for the entire Racket Science Collection,
;;; excluding the graphics routines. (Note, the file science-with-graphics.ss
;;; includes the graphics routines.)
;;;
;;; Version  Date      Description
;;; 1.0.0    09/20/04  The initial version of the file. Includes all of the
;;;                    sub-collections for Release 1.0. (MDW)
;;; 2.0.0    06/07/08  More V4.0 changes. (MDW)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "machine.rkt")
(require "math.rkt")
(require "special-functions.rkt")
(require "random-source.rkt")
(require "random-distributions.rkt")
(require "statistics.rkt")
(require "histogram.rkt")
(require "histogram-2d.rkt")
(require "discrete-histogram.rkt")
(require "chebyshev.rkt")
(require "ode-initval.rkt")
(require "fft.rkt")
(require "constants.rkt")

(provide
 (all-from-out "machine.rkt")
 (all-from-out "math.rkt")
 (all-from-out "special-functions.rkt")
 (all-from-out "random-source.rkt")
 (all-from-out "random-distributions.rkt")
 (all-from-out "statistics.rkt")
 (all-from-out "histogram.rkt")
 (all-from-out "histogram-2d.rkt")
 (all-from-out "discrete-histogram.rkt")
 (all-from-out "chebyshev.rkt")
 (all-from-out "ode-initval.rkt")
 (all-from-out "fft.rkt")
 (all-from-out "constants.rkt"))
