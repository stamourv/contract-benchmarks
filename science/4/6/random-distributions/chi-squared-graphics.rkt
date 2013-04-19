#lang racket/gui
;;; Science Collection
;;; random-distributions/chi-squared-graphics.rkt
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
;;; This code implements graphics for chi-squared distributions.
;;;
;;; Version  Date      Description
;;; 0.1.0    09/17/04  This is the initial release of the chi-squared
;;;                    distribution graphics routines. (Doug Williams)
;;; 1.0.0    09/28/04  Added contracts for functions.  Marked as ready
;;;                    for Release 1.0.  (Doug Williams)
;;; 1.1.0    02/09/06  Added cdf.  (Doug Williams)
;;; 2.0.0    11/19/07  Changed calls to unchecked versions.
;;;                    (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changes x axis label.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Updated to new plot package. (MDW)

(require plot
         "chi-squared.ss")

(define (chi-squared-plot nu)
  (plot (list (function (lambda (x) (chi-squared-pdf x nu))
                        #:label "y = pdf(x)"
                        #:color 'red)
              (function (lambda (x) (chi-squared-cdf x nu))
                        #:label "y = cdf(x)"
                        #:color 'blue))
        #:x-min 0.0 #:x-max 10.0 #:x-label "x"
        #:y-label "Density"
        #:title "Chi-Squared Distribution"))

;;; Module Contracts

(provide/contract
 (chi-squared-plot
  (-> real? (or/c (is-a?/c image-snip%) void?))))
