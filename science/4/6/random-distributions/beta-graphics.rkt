#lang racket/gui
;;; Science Collection
;;; random-distributions/beta-graphics.rkt
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
;;; This code implements graphics for beta distributions.
;;;
;;; Version  Date      Description
;;; 0.1.0    09/17/04  This is the initial release of the beta distribution
;;;                    graphics routines. (MDW)
;;; 1.0.0    09/28/04  Added contracts for functions. Marked as ready for
;;;                    Release 1.0. (MDW)
;;; 1.1.0    02/08/06  Added cdf. (MDW)
;;; 2.0.0    11/19/07  Changed calls to unchecked versions. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 3.0.1    07/01/08  Changed x axis label. (MDW)
;;; 4.0.0    06/11/10  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Updated to the new plot package. (MDW)

(require plot
         "beta.rkt")

;;; (beta-plot a b) -> (or/c (is-a?/c image-snip%) void?))
;;;   a : real?
;;;   b : real?
(define (beta-plot a b)
  (plot (list (function (lambda (x) (beta-pdf x a b))
                        #:label "y = pdf(x)"
                        #:color "red")
              (function (lambda (x) (beta-cdf x a b))
                        #:label "y = cdf(x)"
                        #:color "blue"))
        #:x-min 0 #:x-max 1 #:x-label "x"
        #:y-label "Density"
        #:title "Beta Distribution"))

;;; Module Contracts

(provide/contract
 (beta-plot
  (-> real? real? (or/c (is-a?/c image-snip%) void?))))
