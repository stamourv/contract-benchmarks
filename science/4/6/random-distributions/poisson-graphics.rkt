#lang racket/gui
;;; Science Collection
;;; poisson-graphics.rkt
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
;;; This module implements the graphics for the Bernoulli distribution.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Added contracts for functions.  Marked as ready
;;;                    for Release 1.0.  (Doug Williams)
;;; 2.0.0    11/19/07  Changed calls to unchecked versions.
;;;                    (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changed x axis label.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Updated to the new plot package. (MDW)

(require plot
         "poisson.rkt")

(define (poisson-plot mu)
  (let ((cat-vals1
         (for/list ((i (in-range (* mu 3))))
           (vector i (poisson-pdf i mu))))
        (cat-vals2
         (for/list ((i (in-range (* mu 3))))
           (vector i (poisson-cdf i mu)))))
    (plot (list (discrete-histogram cat-vals2
                                    #:label "y = cdf(x)"
                                    #:color 3 #:line-color 3
                                    #:alpha 0.5)
                (discrete-histogram cat-vals1
                                    #:label "y = pdf(x)"
                                    #:color 1 #:line-color 1
                                    #:alpha 0.5))
          #:x-label "x"
          #:y-label "Density"
          #:title "Poisson Distribution")))

;;; Module Contracts

(provide/contract
 (poisson-plot
  (-> (>/c 0.0) (or/c (is-a?/c image-snip%) void?))))
