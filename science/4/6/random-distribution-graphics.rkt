#lang racket/base
;;; Science Collection
;;; random-distribution-graphics.rkt
;;; Copyright (c) 2004-2011 M. Douglas Williams
;;;
;;; This library is free software; you can redistribute it and/or 
;;; modify it under the terms of the GNU Lesser General Public 
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, 
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA.
;;;
;;; -------------------------------------------------------------------
;;;
;;; This module provides graphics for all of the random distributions
;;; for the PLT Scheme Science Collection as a single module.
;;;
;;; Version  Date      Description
;;; 0.1.0    08/07/04  This is the initial release of the random
;;;                    distributions graphics module.  It will retain
;;;                    the 0.1.0 version until all of the 
;;;                    distributions are defined. (Doug Williams)
;;; 1.0.0    09/20/04  Mark as ready for Release 1.0.  (Doug
;;;                    Williams)
;;; 1.0.1    05/07/07  Added missing graphics requires and provides.
;;;                    (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "random-distributions/beta-graphics.ss")
(require "random-distributions/bivariate-gaussian-graphics.ss")
(require "random-distributions/chi-squared-graphics.ss")
(require "random-distributions/exponential-graphics.ss")
(require "random-distributions/f-distribution-graphics.ss")
(require "random-distributions/flat-graphics.ss")
(require "random-distributions/gamma-graphics.ss")
(require "random-distributions/gaussian-graphics.ss")
(require "random-distributions/gaussian-tail-graphics.ss")
(require "random-distributions/lognormal-graphics.ss")
(require "random-distributions/pareto-graphics.ss")
(require "random-distributions/t-distribution-graphics.ss")
(require "random-distributions/triangular-graphics.ss")
(require "random-distributions/bernoulli-graphics.ss")
(require "random-distributions/binomial-graphics.ss")
(require "random-distributions/geometric-graphics.ss")
(require "random-distributions/logarithmic-graphics.ss")
(require "random-distributions/poisson-graphics.ss")
(require "random-distributions/discrete-graphics.ss")

(provide
 (all-from-out "random-distributions/beta-graphics.ss")
 (all-from-out "random-distributions/bivariate-gaussian-graphics.ss")
 (all-from-out "random-distributions/chi-squared-graphics.ss")
 (all-from-out "random-distributions/exponential-graphics.ss")
 (all-from-out "random-distributions/f-distribution-graphics.ss")
 (all-from-out "random-distributions/gamma-graphics.ss")
 (all-from-out "random-distributions/gaussian-graphics.ss")
 (all-from-out "random-distributions/flat-graphics.ss")
 (all-from-out "random-distributions/gaussian-tail-graphics.ss")
 (all-from-out "random-distributions/lognormal-graphics.ss")
 (all-from-out "random-distributions/pareto-graphics.ss")
 (all-from-out "random-distributions/t-distribution-graphics.ss")
 (all-from-out "random-distributions/triangular-graphics.ss")
 (all-from-out "random-distributions/bernoulli-graphics.ss")
 (all-from-out "random-distributions/binomial-graphics.ss")
 (all-from-out "random-distributions/geometric-graphics.ss")
 (all-from-out "random-distributions/logarithmic-graphics.ss")
 (all-from-out "random-distributions/poisson-graphics.ss")
 (all-from-out "random-distributions/discrete-graphics.ss"))
