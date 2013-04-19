#lang racket/base
;;; Science Collection
;;; random-distributions.rkt
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
;;; This module provides all of the random distributions for the PLT
;;; Science Collection as a single module.
;;;
;;; Version  Date      Description
;;; 0.1.0    08/05/04  This is the initial release of the random
;;;                    distributions module. It will retain the 0.1.0
;;;                    version until all of the distributions for
;;;                    Release 1.0 are defined. (Doug Williams)
;;; 1.0.0    09/20/04  Marked as ready for Release 1.0.  (Doug
;;;                    Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)

(require "random-source.ss")
(require "random-distributions/beta.ss")
(require "random-distributions/bivariate-gaussian.ss")
(require "random-distributions/chi-squared.ss")
(require "random-distributions/exponential.ss")
(require "random-distributions/f-distribution.ss")
(require "random-distributions/flat.ss")
(require "random-distributions/gamma.ss")
(require "random-distributions/gaussian.ss")
(require "random-distributions/gaussian-tail.ss")
(require "random-distributions/lognormal.ss")
(require "random-distributions/pareto.ss")
(require "random-distributions/t-distribution.ss")
(require "random-distributions/triangular.ss")
(require "random-distributions/bernoulli.ss")
(require "random-distributions/binomial.ss")
(require "random-distributions/geometric.ss")
(require "random-distributions/logarithmic.ss")
(require "random-distributions/poisson.ss")
(require "random-distributions/discrete.ss")

(provide
 (all-from-out "random-source.ss")
 (all-from-out "random-distributions/beta.ss")
 (all-from-out "random-distributions/bivariate-gaussian.ss")
 (all-from-out "random-distributions/chi-squared.ss")
 (all-from-out "random-distributions/exponential.ss")
 (all-from-out "random-distributions/f-distribution.ss")
 (all-from-out "random-distributions/gamma.ss")
 (all-from-out "random-distributions/gaussian.ss")
 (all-from-out "random-distributions/flat.ss")
 (all-from-out "random-distributions/gaussian-tail.ss")
 (all-from-out "random-distributions/lognormal.ss")
 (all-from-out "random-distributions/pareto.ss")
 (all-from-out "random-distributions/t-distribution.ss")
 (all-from-out "random-distributions/triangular.ss")
 (all-from-out "random-distributions/bernoulli.ss")
 (all-from-out "random-distributions/binomial.ss")
 (all-from-out "random-distributions/geometric.ss")
 (all-from-out "random-distributions/logarithmic.ss")
 (all-from-out "random-distributions/poisson.ss")
 (all-from-out "random-distributions/discrete.ss"))
