#lang racket/gui
;;; Science Collection
;;; random-distributions/triangular-graphics.rkt
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
;;; This code implements graphics for triangular distributions.
;;;
;;; Version  Date      Description
;;; 0.1.0    08/07/04  This is the initial release of the triangular
;;;                    distribution graphics routines. (Doug Williams)
;;; 1.0.0    09/28/04  Added contracts for functions.  Marked as ready
;;;                    for Release 1.0.  (Doug Williams)
;;; 2.0.0    11/19/07  Changed calls to unchecked versions.
;;;                    (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changed x axis label.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Updated to new plot package. (MDW)

(require plot
         "triangular.rkt")

;;; triangular-plot: real x real x real -> 2d-view%
;;; Plot the pdf and cdf for a traingular distribution with the given
;;; parameters.  The x axis range is a to b.
(define (triangular-plot a b c)
  (plot (list (function (lambda (x) (triangular-pdf x a b c))
                        #:label "y = pdf(x)"
                        #:color "red")
              (function (lambda (x) (triangular-cdf x a b c))
                        #:label "y = cdf(x)"
                        #:color "blue"))
        #:x-min a #:x-max b #:x-label "x"
        #:y-label "Density"
        #:title "Triangular Distribution"))

;;; Module Contracts

(provide/contract
 (triangular-plot
  (->i ((a real?)
        (b (a) (>/c a))
        (c (a b) (and/c (>=/c a)
                        (<=/c b))))
       any)))
