#lang racket/base
;;; Science Collection
;;; science-with-graphics.rkt
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
;;; including the graphics routines.  (Note, the file science.ss excludes the
;;; graphics routines.)
;;;
;;; Version  Date      Description
;;; 1.0.0    09/20/04  The initial version of the file. Includes all of the
;;;                    sub-collections for Release 1.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "science.rkt")
(require "random-distribution-graphics.rkt")
(require "histogram-graphics.rkt")
(require "histogram-2d-graphics.rkt")
(require "discrete-histogram-graphics.rkt")

(provide
 (all-from-out "science.rkt")
 (all-from-out "random-distribution-graphics.rkt")
 (all-from-out "histogram-graphics.rkt")
 (all-from-out "histogram-2d-graphics.rkt")
 (all-from-out "discrete-histogram-graphics.rkt"))
