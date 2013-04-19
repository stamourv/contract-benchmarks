#lang racket/base
;;; PLT Scheme Science Collection
;;; random-distributions-with-graphics.rkt
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
;;; Scheme Science Collection and their associated graphics as a
;;; single module.
;;;
;;; Version  Date      Description
;;; 0.9.0    08/07/04  This is the initial release of the random
;;;                    distributions with graphics module. (Doug
;;;                    Williams)
;;; 1.0.0    09/20/04  Marked as ready for Release 1.0.  (Doug
;;;                    Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "random-distributions.rkt")
(require "random-distribution-graphics.rkt")

(provide
 (all-from-out "random-distributions.rkt")
 (all-from-out "random-distribution-graphics.rkt"))
