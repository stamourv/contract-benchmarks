#lang racket
;;; Science Collection
;;; test-constants.rkt
;;; Copyright (c) 2010-2011 M. Douglas Williams
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
;;; This file provides simple tests for the physical constants modules. A sample
;;; from each module is tested, basically to make sure the modules load more than
;;; anything else.
;;;
;;; Version  Date      Description
;;; 1.0.0    04/21/11  Added tests for constants. (MDW)
;;; 

(require ;; (planet williams/science/constants)
         ;; (planet williams/science/math)
         "constants.rkt" "math.rkt"
         rackunit)

(define-simple-check (check-rel actual expected epsilon)
  (if (zero? expected)
      (<= (abs actual) epsilon)
      (<= (/ (abs (- actual expected)) (abs expected)) epsilon)))
  
(let ((c mks-speed-of-light)
      (eps mks-vacuum-permittivity)
      (mu mks-vacuum-permeability))
  (check-rel (/ 1.0 (sqrt (* eps mu))) c 1e-10))

(let ((ly cgs-light-year)
      (c cgs-speed-of-light)
      (y (* 365.2425  cgs-day)))
  (check-rel (* c y) ly 1e-10))

(let ((c mksa-speed-of-light)
      (eps mksa-vacuum-permittivity)
      (mu mksa-vacuum-permeability))
  (check-rel (/ 1.0 (sqrt (* eps mu))) c 1e-10))

(let ((ly cgsm-light-year)
      (c cgsm-speed-of-light)
      (y (* 365.2425 cgsm-day)))
  (check-rel (* c y) ly 1e-10))

(let ((micro num-micro)
      (mega num-mega)
      (kilo num-kilo))
  (check-rel (/ mega kilo) (/ 1.0 (* micro kilo)) 1e-10))

(let* ((d mksa-debye)
       (c mksa-speed-of-light)
       (desu (* d c 1000.0)))
  (check-rel desu 1e-18 1e-10))

(let* ((k mksa-boltzmann)
       (c mksa-speed-of-light)
       (h mksa-plancks-constant-h)
       (s (/ (* 2.0 (expt pi 5.0) (expt k 4.0))
             (* 15.0 (expt c 2.0) (expt h 3.0))))
       (sigma mksa-stefan-boltzmann-constant))
  (check-rel s sigma 1e-10))