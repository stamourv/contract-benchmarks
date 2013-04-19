#lang racket/base
;;; Science Collection
;;; cgsm-constants.rkt
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
;;; This code is based on the CGSM constants in the GNU Scientific Library (GSL),
;;; which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 4.0.0    05/22/10  Initial release of the CGSM constants. (MDW)

(define cgsm-speed-of-light 2.99792458e10) ; cm / s
(define cgsm-gravitational-constant 6.673e-8) ; cm^3 / g s^2
(define cgsm-plancks-constant-h 6.62606896e-27) ; g cm^2 / s
(define cgsm-plancks-constant-hbar 1.05457162825e-27) ; g cm^2 / s
(define cgsm-astronomical-unit 1.49597870691e13) ; cm
(define cgsm-light-year 9.46053620707e17) ; cm
(define cgsm-parsec 3.08567758135e18) ; cm
(define cgsm-grav-accel 9.80665e2) ; cm / s^2
(define cgsm-electron-volt 1.602176487e-12) ; g cm^2 / s^2
(define cgsm-mass-electron 9.10938188e-28) ; g
(define cgsm-mass-muon 1.88353109e-25) ; g
(define cgsm-mass-proton 1.67262158e-24) ; g
(define cgsm-mass-neutron 1.67492716e-24) ; g
(define cgsm-rydberg 2.17987196968e-11) ; g cm^2 / s^2
(define cgsm-boltzmann 1.3806504e-16) ; g cm^2 / K s^2
(define cgsm-molar-gas 8.314472e7) ; g cm^2 / K mol s^2
(define cgsm-standard-gas-volume 2.2710981e4) ; cm^3 / mol
(define cgsm-minute 6e1) ; s
(define cgsm-hour 3.6e3) ; = (* 60 cgsm-minute) s
(define cgsm-day 8.64e4) ; = (* 24 cgsm-hour) s
(define cgsm-week 6.048e5) ; = (* 7 cgsm-day) s
(define cgsm-inch 2.54e0) ; cm
(define cgsm-foot 3.048e1) ; = (* 12 cgsm-inch) cm
(define cgsm-yard 9.144e1) ; = (* 3 cgsm-foot) cm
(define cgsm-mile 1.609344e5) ; = (* 1760 cgsm-yard) cm
(define cgsm-nautical-mile 1.852e5) ; = (* 1000 1852.0) cm
(define cgsm-fathom 1.8288e2) ; = (* 2 cgsm-yard) cm
(define cgsm-mil 2.54e-3) ; = (/ cgsm-inch 1000) cm
(define cgsm-point 3.52777777778e-2) ; = (* 1/72 cgsm-inch) cm
(define cgsm-texpoint 3.51459803515e-2) ; = (* (/ 1 72.27) cgsm-inch) cm
(define cgsm-micron 1e-4) ; cm
(define cgsm-angstrom 1e-8) ; cm
(define cgsm-hectare 1e8) ; cm^2
(define cgsm-acre 4.04685642241e7) ; cm^2
(define cgsm-barn 1e-24) ; cm^2
(define cgsm-liter 1e3) ; cm^3
(define cgsm-us-gallon 3.78541178402e3) ; = (* 4 cgsm-quart) cm^3
(define cgsm-quart 9.46352946004e2) ; = (* 2 cgsm-pint) cm^3
(define cgsm-pint 4.73176473002e2) ; = (* 2 cgsm-cup) cm^3
(define cgsm-cup 2.36588236501e2) ; = (* 8 cgsm-fluid-ounce) cm^3
(define cgsm-fluid-ounce 2.95735295626e1) ; = (* 2 cgsm-tablespoon) cm^3
(define cgsm-tablespoon 1.47867647813e1) ; = (* 3 cgsm-teaspoon) cm^3
(define cgsm-teaspoon 4.92892159375e0) ; cm^3
(define cgsm-canadian-gallon 4.54609e3) ; cm^3
(define cgsm-uk-gallon 4.546092e3) ; cm^3
(define cgsm-miles-per-hour 4.4704e1) ; = (/ cgsm-mile cgsm-hour) cm / s
(define cgsm-kilometers-per-hour 2.77777777778e1) ; = (/ 1e4 cgsm-hour) cm / s
(define cgsm-knot 5.14444444444e1) ; = (/ cgsm-nautical-mile cgsm-hour) cm / s
(define cgsm-pound-mass 4.5359237e2) ; g
(define cgsm-ounce-mass 2.8349523125e1) ; = (/ cgsm-pound-mass 16) g
(define cgsm-ton 9.0718474e5) ; = (* 2000 cgsm-pound-mass) g
(define cgsm-metric-ton 1e6) ; g
(define cgsm-uk-ton 1.0160469088e6) ; = (* 2240 cgsm-pound-mass) g
(define cgsm-troy-ounce 3.1103475e1) ; g
(define cgsm-carat 2e-1) ; g
(define cgsm-unified-atomic-mass 1.660538782e-24) ; g
(define cgsm-gram-force 9.80665e2) ; = (* 1 cgsm-grav-accel) cm g / s^2
(define cgsm-pound-force 4.44822161526e5) ; = (* cgsm-pound-mass cgsm-grav-accel) cm g / s^2
(define cgsm-kilopound-force 4.44822161526e8) ; = (* 1000 cgsm-pound-force) cm g / s^2
(define cgsm-poundal 1.38255e4) ; = (* cgsm-pound-mass cgsm-foot) cm g / s^2
(define cgsm-calorie 4.1868e7) ; g cm^2 / s^2
(define cgsm-btu 1.05505585262e10) ; g cm^2 / s^2
(define cgsm-therm 1.05506e15) ; g cm^2 / s^2
(define cgsm-horsepower 7.45699872e9) ; g cm^2 / s^3
(define cgsm-bar 1e6) ; g / cm s^2
(define cgsm-std-atmosphere 1.01325e6) ; g / cm s^2
(define cgsm-torr 1.33322368421e3) ; g / cm s^2
(define cgsm-meter-of-mercury 1.33322368421e6) ; g / cm s^2
(define cgsm-inch-of-mercury 3.38638815789e4) ; g / cm s^2
(define cgsm-inch-of-water 2.490889e3) ;  g / cm s^2
(define cgsm-psi 6.89475729317e4) ; g / cm s^2
(define cgsm-poise 1e0) ; g / cm s
(define cgsm-stokes 1e0) ; cm^2 / s
(define cgsm-stilb 1e0) ; cd / cm^2
(define cgsm-lumen 1e0) ; cd sr
(define cgsm-lux 1e-4) ; cd sr / cm^2
(define cgsm-phot 1e0) ; cd sr / cm^2
(define cgsm-footcandle 1.076e-3) ; cd sr / cm^2
(define cgsm-lambert 1e0) ; cd sr / cm^2
(define cgsm-footlambert 1.07639104e-3) ; cd sr / cm^2
(define cgsm-curie 3.7e10) ; 1 / s
(define cgsm-roentgen 2.58e-7) ; A s / g
(define cgsm-rad 1e2) ; cm^2 / s^2
(define cgsm-solar-mass 1.98892e33) ; g
(define cgsm-bohr-radius 5.291772083e-9) ; cm
(define cgsm-newton 1e5) ; cm g /s^2
(define cgsm-dyne 1e0) ; cm g / s^2
(define cgsm-joule 1e7) ; g cm^2 / s^2
(define cgsm-erg 1e0) ; g cm^2 / s^2
(define cgsm-stefan-boltzmann-constant 5.67040047374e-5) ; g / K^4 s^3
(define cgsm-thomson-cross-section 6.65245893699e-25) ; cm^2
(define cgsm-bohr-magneton 9.27400899e-21) ; abamp cm^2
(define cgsm-nuclear-magneton 5.05078317e-24) ; abamp cm^2
(define cgsm-electron-magnetic-moment 9.28476362e-21) ; abamp cm^2
(define cgsm-proton-magnetic-moment 1.410606633e-23) ; abamp cm^2
(define cgsm-faraday 9.64853429775e3) ; abamp s / mol
(define cgsm-electron-charge 1.602176487e-20) ; abamp s

;;; Module Contracts

(provide (all-defined-out))
