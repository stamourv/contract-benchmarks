#lang racket/gui
;;; Science Collection
;;; random-distributions/bivariate-gaussian-graphics.rkt
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
;;; This code implements graphics for bivariate gaussian distributions.
;;;
;;; Version  Date      Description
;;; 0.1.0    08/07/04  This is the initial release of the bivariate Gaussian
;;;                    distribution graphics routines. (MDW)
;;; 1.0.0    09/28/04  Added contracts for functions. Marked as ready for
;;;                    Release 1.0. (MDW)
;;; 2.0.0    11/19/07  Changed calls to unchecked versions. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 3.0.1    07/01/08  Changes axes labels. (MDW)
;;; 4.0.0    06/11/10  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Updated to the new plot package

(require plot
         "bivariate-gaussian.rkt")

;;; gaussian-plot: real x real -> void
;;; Plot the pdf and cdf for a gaussian distribution with the given
;;; mean and standard deviation.  The x axis range is +/- three sigma
;;; around the mean.
(define (bivariate-gaussian-plot sigma-x sigma-y rho)
  (plot3d (surface3d (lambda (x y) 
                       (bivariate-gaussian-pdf 
                        x y sigma-x sigma-y rho))
                     #:label "z = pdf(x,y)"
                     #:color 1 #:line-color 1)
          #:x-min (* -3 sigma-x) #:x-max (* 3 sigma-x) #:x-label "x"
          #:y-min (* -3 sigma-y) #:y-max (* 3 sigma-y) #:y-label "y"
          #:z-label "Density"
          #:title "Bivariate Gaussian Distribution"))

;;; Module Contracts

(provide/contract
 (bivariate-gaussian-plot
  (-> (>=/c 0.0) (>=/c 0.0) (real-in -1.0 1.0)
      (or/c (is-a?/c image-snip%) void?))))
