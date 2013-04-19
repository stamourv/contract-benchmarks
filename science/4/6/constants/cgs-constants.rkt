#lang racket/base
;;; Science Collection
;;; cgs-constants.rkt
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
;;; This code is based on the CGS constants in the GNU Scientific Library (GSL),
;;; which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 4.0.0    05/22/10  Initial release of the CGS constants. (MDW)

(define cgs-speed-of-light 2.99792458e10) ; cm / s
(define cgs-gravitational-constant 6.673e-8) ; cm^3 / g s^2
(define cgs-plancks-constant-h 6.62606896e-27) ; g cm^2 / s
(define cgs-plancks-constant-hbar 1.05457162825e-27) ; g cm^2 / s
(define cgs-astronomical-unit 1.49597870691e13) ; cm
(define cgs-light-year 9.46053620707e17) ; cm
(define cgs-parsec 3.08567758135e18) ; cm
(define cgs-grav-accel 9.80665e2) ; cm / s^2
(define cgs-electron-volt 1.602176487e-12) ; g cm^2 / s^2
(define cgs-mass-electron 9.10938188e-28) ; g
(define cgs-mass-muon 1.88353109e-25) ; g
(define cgs-mass-proton 1.67262158e-24) ; g
(define cgs-mass-neutron 1.67492716e-24) ; g
(define cgs-rydberg 2.17987196968e-11) ; g cm^2 / s^2
(define cgs-boltzmann 1.3806504e-16) ; g cm^2 / K s^2
(define cgs-molar-gas 8.314472e7) ; g cm^2 / K mol s^2
(define cgs-standard-gas-volume 2.2710981e4) ; cm^3 / mol
(define cgs-minute 6e1) ; s
(define cgs-hour 3.6e3) ; = (* 60 cgs-minute) s
(define cgs-day 8.64e4) ; = (* 24 cgs-hour) s
(define cgs-week 6.048e5) ; = (* 7 cgs-day) s
(define cgs-inch 2.54e0) ; cm
(define cgs-foot 6.048e5) ; = (* 12 cgs-inch) cm
(define cgs-yard 9.144e1) ; = (* 3 cgs-foot) cm
(define cgs-mile 1.609344e5) ; = (* 1760 cgs-yard) cm
(define cgs-nautical-mile 1.852e5) ; = (* 1000 1852.0) cm
(define cgs-fathom 1.8288e2) ; = (* 2 cgs-yard) cm
(define cgs-mil 2.54e-3) ; = (/ cgs-inch 1000) cm
(define cgs-point 3.52777777778e-2) ; = (* 1/72 cgs-inch) cm
(define cgs-texpoint 3.51459803515e-2) ; = (* (/ 1 72.27) cgs-inch) cm
(define cgs-micron 1e-4) ; cm
(define cgs-angstrom 1e-8) ; cm
(define cgs-hectare 1e8) ; cm^2
(define cgs-acre 4.04685642241e7) ; cm^2
(define cgs-barn 1e-24) ; cm^2
(define cgs-liter 1e3) ; cm^3
(define cgs-us-gallon 3.78541178402e3) ; = (* 4 cgs-quart) cm^3
(define cgs-quart 9.46352946004e2) ; = (* 2 cgs-pint) cm^3
(define cgs-pint 4.73176473002e2) ; = (* 2 cgs-cup) cm^3
(define cgs-cup 2.36588236501e2) ; = (* 8 cgs-fluid-ounce) cm^3
(define cgs-fluid-ounce 2.95735295626e1) ; = (* 2 cgs-tablespoon) cm^3
(define cgs-tablespoon 1.47867647813e1) ; = (* 3 cgs-teaspoon)  cm^3
(define cgs-teaspoon 4.92892159375e0) ; cm^3
(define cgs-canadian-gallon 4.54609e3) ; cm^3
(define cgs-uk-gallon 4.546092e3) ; cm^3
(define cgs-miles-per-hour 4.4704e1) ; = (/ cgs-mile cgs-hour) cm / s
(define cgs-kilometers-per-hour 2.77777777778e1) ; = (/ 1e4 cgs-hour) cm / s
(define cgs-knot 5.14444444444e1) ; = (/ cgs-nautical-mile cgs-hour) cm / s
(define cgs-pound-mass 4.5359237e2) ; g
(define cgs-ounce-mass 2.8349523125e1) ; = (/ cgs-pound-mass 16) g
(define cgs-ton 9.0718474e5) ; = (* 2000 cgs-pound-mass) g
(define cgs-metric-ton 1e6) ; g
(define cgs-uk-ton 1.0160469088e6) ; = (* 2240 cgs-pound-mass) g
(define cgs-troy-ounce 3.1103475e1) ; g
(define cgs-carat 2e-1) ; g
(define cgs-unified-atomic-mass 1.660538782e-24) ; g
(define cgs-gram-force 9.80665e2) ; = (* 1 cgs-grav-accel) cm g / s^2
(define cgs-pound-force 4.44822161526e5) ; = (* cgs-pound-mass cgs-grav-accel) cm g / s^2
(define cgs-kilopound-force 4.44822161526e8) ; = (* 1000 cgs-pound-force) cm g / s^2
(define cgs-poundal 1.38255e4) ; = (* cgs-pound-mass cgs-foot) cm g / s^2
(define cgs-calorie 4.1868e7) ; g cm^2 / s^2
(define cgs-btu 1.05505585262e10) ; g cm^2 / s^2
(define cgs-therm 1.05506e15) ; g cm^2 / s^2
(define cgs-horsepower 7.45699872e9) ; g cm^2 / s^3
(define cgs-bar 1e6) ; g / cm s^2
(define cgs-std-atmosphere 1.01325e6) ; g / cm s^2
(define cgs-torr 1.33322368421e3) ; g / cm s^2
(define cgs-meter-of-mercury 1.33322368421e6) ; g / cm s^2
(define cgs-inch-of-mercury 3.38638815789e4) ; g / cm s^2
(define cgs-inch-of-water 2.490889e3) ;  g / cm s^2
(define cgs-psi 6.89475729317e4) ; g / cm s^2
(define cgs-poise 1e0) ; g / cm s
(define cgs-stokes 1e0) ; cm^2 / s
(define cgs-stilb 1e0) ; cd / cm^2
(define cgs-lumen 1e0) ; cd sr
(define cgs-lux 1e-4) ; cd sr / cm^2
(define cgs-phot 1e0) ; cd sr / cm^2
(define cgs-footcandle 1.076e-3) ; cd sr / cm^2
(define cgs-lambert 1e0) ; cd sr / cm^2
(define cgs-footlambert 1.07639104e-3) ; cd sr / cm^2
(define cgs-curie 3.7e10) ; 1 / s
(define cgs-roentgen 2.58e-7) ; A s / g
(define cgs-rad 1e2) ; cm^2 / s^2
(define cgs-solar-mass 1.98892e33) ; g
(define cgs-bohr-radius 5.291772083e-9) ; cm
(define cgs-newton 1e5) ; cm g /s^2
(define cgs-dyne 1e0) ; cm g / s^2
(define cgs-joule 1e7) ; g cm^2 / s^2
(define cgs-erg 1e0) ; g cm^2 / s^2
(define cgs-stefan-boltzmann-constant 5.67040047374e-5) ; g / K^4 s^3
(define cgs-thomson-cross-section 6.65245893699e-25) ; cm^2

;;; Module Contracts

(provide (all-defined-out))
