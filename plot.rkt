#lang racket/base

;; measures contracts in plot and racket/draw
;; generates a 3D plot and converts it to a png
;; contains frozen versions of both plot and racket/draw (and dependencies)

(require "plot/main.rkt"
         (prefix-in file: file/convertible))

(time
 (void
  (file:convert
   (plot3d-pict (isosurfaces3d (compose abs max) -1 1 -1 1 -1 1))
   'png-bytes)))
