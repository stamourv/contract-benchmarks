#lang racket
;;; Science Collection
;;; discrete-histogram-with-graphics.rkt
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
;;; Version  Date      Description
;;; 1.0.0    09/30/04  Marked as ready for Release 1.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

(require "discrete-histogram.rkt"
         "discrete-histogram-graphics.rkt")

(provide
 (all-from-out "discrete-histogram.rkt"
               "discrete-histogram-graphics.rkt"))
