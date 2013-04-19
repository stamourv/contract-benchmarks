#lang setup/infotab
(define name "Science")
(define blurb
  (list "The Science Collection is a collection of modules that provide "
        "functions for numerical computing. The structure of the science "
        "collection and many of the underlying algorithms were inspired by the "
        "GNU Scientific Library (GSL). The functions are written entirely in "
        "Racket and present a true Racket look-and-feel throughout. The source "
        "code is distributed with the science collection and licensed under the "
        "GNU Lesser General Public License (LGPL), Version 3.0."))
(define release-notes
  (list "Fixed a bug in ldexp and fcmp where a value used in unsafe fl "
        "operations was not protected."))
(define categories '(scientific))
(define primary-file '("science.rkt" "science-with-graphics.rkt"))
(define required-core-version "5.2")
(define repositories
  (list "4.x"))
(define category '(library))
(define scribblings '(("scribblings/science.scrbl" (multi-page))))
(define compile-omit-files '("ode-initval/rk2.rkt"
                             "ode-initval/rk4.rkt"
                             "ode-initval/rkf45.rkt"
                             "special-functions/gamma-imp.rkt"
                             "special-functions/gamma-inc-imp.rkt"
                             "special-functions/psi-imp.rkt"
                             "special-functions/zeta-imp.rkt"))
