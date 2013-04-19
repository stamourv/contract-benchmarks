#lang racket/gui
;;; Science Collection
;;; random-distributions/pareto-graphics.rkt
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
;;; This module implements the graphics for Pareto distributions.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Added contracts for functions.  Marked as ready
;;;                    for Release 1.0.  (Doug Williams)
;;; 2.0.0    11/19/07  Changed calls to unchecked versions.
;;;                    (Doug Williams)
;;; 3.0.0    06/09/05  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changed x axis label.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Updated to the new plot package. (MDW)

(require "pareto.rkt"
         plot)

;;; pareto-plot: real x real -> void
;;; Plot the pdf and cdf functions for a Pareto distribution with
;;; exponent a and scale b.
(define (pareto-plot a b)
  (plot (list (function (lambda (x) (pareto-pdf x a b))
                        #:label "y = pdf(x)"
                        #:color "red")
              (function (lambda (x) (pareto-cdf x a b))
                        #:label "y = cdf(x)"
                        #:color "blue"))
        #:x-min b #:x-max (+ b 20) #:x-label "x"
        #:y-label "Density"
        #:title "Pareto Distribution"))

;;; Module Contracts

(provide/contract
 (pareto-plot
  (-> real? real? (or/c (is-a?/c image-snip%) void?))))
