#lang racket/base
;;; Science Collection
;;; mks-constants.rkt
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
;;; This code is based on the MKS constants in the GNU Scientific Library (GSL),
;;; which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 4.0.0    07/01/10  Initial release of the MKS constants. (MDW)

(define mks-speed-of-light 2.99792458e8) ; m / s
(define mks-gravitational-constant 6.673e-11) ; m^3 / kg s^2
(define mks-plancks-constant-h 6.62606896e-34) ; kg m^2 / s
(define mks-plancks-constant-hbar 1.05457162825e-34) ; kg m^2 / s
(define mks-astronomical-unit 1.49597870691e113) ; m
(define mks-light-year 9.46053620707e15) ; m
(define mks-parsec 3.08567758135e16) ; m
(define mks-grav-accel 9.80665e0) ; m / s^2
(define mks-electron-volt 1.602176487e-19) ; kg m^2 / s^2
(define mks-mass-electron 9.10938188e-31) ; kg
(define mks-mass-muon 1.88353109e-28) ; kg
(define mks-mass-proton 1.67262158e-27) ; kg
(define mks-mass-neutron 1.67492716e-27) ; kg
(define mks-rydberg 2.17987196968e-18) ; kg m^2 / s^2
(define mks-boltzmann 1.3806504e-23) ; kg m^2 / K s^2
(define mks-molar-gas 8.314472e0) ; kg m^2 / K mol s^2
(define mks-standard-gas-volume 2.2710981e-2) ; m^3 / mol
(define mks-minute 6e1) ; s
(define mks-hour 3.6e3) ; s
(define mks-day 8.64e4) ; s
(define mks-week 6.048e5) ; s
(define mks-inch 2.54e-2) ; m
(define mks-foot 3.048e-1) ; m
(define mks-yard 9.144e-1) ; m
(define mks-mile 1.609344e3) ; m
(define mks-nautical-mile 1.852e3) ; m
(define mks-fathom 1.8288e0) ; m
(define mks-mil 2.54e-5) ; m
(define mks-point 3.52777777778e-4) ; m
(define mks-texpoint 3.51459803515e-4) ; m
(define mks-micron 1e-6) ; m
(define mks-angstrom 1e-10) ; m
(define mks-hectare 1e4) ; m^2
(define mks-acre 4.04685642241e3) ; m^2
(define mks-barn 1e-28) ; m^2
(define mks-liter 1e-3) ; m^3
(define mks-us-gallon 3.78541178402e-3) ; m^3
(define mks-quart 9.46352946004e-4) ; m^3
(define mks-pint 4.73176473002e-4) ; m^3
(define mks-cup 2.36588236501e-4) ; m^3
(define mks-fluid-ounce 2.95735295626e-5) ; m^3
(define mks-tablespoon 1.47867647813e-5) ; m^3
(define mks-teaspoon 4.92892159375e-6) ; m^3
(define mks-canadian-gallon 4.54609e-3) ; m^3
(define mks-uk-gallon 4.546092e-3) ; m^3
(define mks-miles-per-hour 4.4704e-1) ; m / s
(define mks-kilometers-per-hour 2.77777777778e-1) ; m / s
(define mks-knot 5.14444444444e-1) ; m / s
(define mks-pound-mass 4.5359237e1) ; kg
(define mks-ounce-mass 2.8349523125e-2) ; kg
(define mks-ton 9.0718474e2) ; kg
(define mks-metric-ton 1e3) ; kg
(define mks-uk-ton 1.0160469088e3) ; kg
(define mks-troy-ounce 3.1103475e-2) ; kg
(define mks-carat 2e-4) ; kg
(define mks-unified-atomic-mass 1.660538782e-27) ; kg
(define mks-gram-force 9.80665e-3) ; m kg / s^2
(define mks-pound-force 4.44822161526e0) ; m kg / s^2
(define mks-kilopound-force 4.44822161526e3) ; m kg / s^2
(define mks-poundal 1.38255e-1) ; m kg / s^2
(define mks-calorie 4.1868e0) ; kg m^2 / s^2
(define mks-btu 1.05505585262e3) ; kg m^2 / s^2
(define mks-therm 1.05506e8) ; kg m^2 / s^2
(define mks-horsepower 7.45699872e2) ; kg m^2 / s^3
(define mks-bar 1e5) ; kg / m s^2
(define mks-std-atmosphere 1.01325e5) ; kg / m s^2
(define mks-torr 1.33322368421e2) ; kg / m s^2
(define mks-meter-of-mercury 1.33322368421e5) ; kg / m s^2
(define mks-inch-of-mercury 3.38638815789e3) ; kg / m s^2
(define mks-inch-of-water 2.490889e2) ;  kg / m s^2
(define mks-psi 6.89475729317e3) ; kg / m s^2
(define mks-poise 1e-1) ; kg / m s
(define mks-stokes 1e-4) ; m^2 / s
(define mks-stilb 1e4) ; cd / m^2
(define mks-lumen 1e0) ; cd sr
(define mks-lux 1e0) ; cd sr / m^2
(define mks-phot 1e4) ; cd sr / m^2
(define mks-footcandle 1.076e1) ; cd sr / m^2
(define mks-lambert 1e4) ; cd sr / m^2
(define mks-footlambert 1.07639104e1) ; cd sr / m^2
(define mks-curie 3.7e10) ; 1 / s
(define mks-roentgen 2.58e-4) ; A s / kg
(define mks-rad 1e-2) ; m^2 / s^2
(define mks-solar-mass 1.98892e30) ; kg
(define mks-bohr-radius 5.291772083e-11) ; m
(define mks-newton 1e0) ; m kg /s^2
(define mks-dyne 1e-5) ; m kg / s^2
(define mks-joule 1e0) ; kg m^2 / s^2
(define mks-erg 1e-7) ; kg m^2 / s^2
(define mks-stefan-boltzmann-constant 5.67040047374e-8) ; kg / K^4 s^3
(define mks-thomson-cross-section 6.65245893699e-29) ; m^2
(define mks-bohr-magneton 9.27400899e-24) ; A m^2
(define mks-nuclear-magneton 5.05078317e-27) ; A m^2
(define mks-electron-magnetic-moment 9.28476362e-24) ; A m^2
(define mks-proton-magnetic-moment 1.410606633e-26) ; A m^2
(define mks-faraday 9.64853429775e4) ; A s / mol
(define mks-electron-charge 1.602176487e-19) ; A s
(define mks-vacuum-permittivity 8.854187817e-12) ; A^2 s^4 / kg m^3
(define mks-vacuum-permeability 1.25663706144e-6) ; kg m / A^2 s^2
(define mks-debye 3.33564095198e-30) ; A s^2 / m^2
(define mks-gauss 1e-4) ; kg / A s^2

;;; Module Contracts

(provide (all-defined-out))
