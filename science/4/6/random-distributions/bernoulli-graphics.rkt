#lang racket/gui
;;; Science Collection
;;; random-distributions/bernoulli-graphics.rkt
;;; Copyright (c) 2004-2010 M. Douglas Williams
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
;;; This module implements the graphics for the Bernoulli distribution.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Added contracts for functions. Marked as ready for
;;;                    Release 1.0. (MDW)
;;; 2.0.0    11/19/07  Changed calls to unchecked versions. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 3.0.1    07/01/08  Changed x axis label. (MDW)
;;; 4.0.0    06/11/10  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Updated to new plot package. (MDW)

(require plot
         "bernoulli.ss")

;;; (bernoulli-plot p) -> (or/c (is-a?/c image-snip%) void?)
;;;   p : real?
(define (bernoulli-plot p)
  (let ((cat-vals1 (list (vector 0 (bernoulli-pdf 0 p))
                         (vector 1 (bernoulli-pdf 1 p))))
        (cat-vals2 (list (vector 0 (bernoulli-cdf 0 p))
                         (vector 1 (bernoulli-cdf 1 p)))))
    (plot (list (discrete-histogram cat-vals2
                                    #:label "y = cdf(x)"
                                    #:color 3 #:line-color 3 #:alpha .5)
                (discrete-histogram cat-vals1
                                    #:label "y = pdf(x)"
                                    #:color 1 #:line-color 1 #:alpha .5))
          #:x-label "x"
          #:y-label "Density"
          #:title "Bernoulli Distribution")))

;;; Module Contracts

(provide/contract
 (bernoulli-plot
  (-> (real-in 0.0 1.0) (or/c (is-a?/c image-snip%) void?))))

