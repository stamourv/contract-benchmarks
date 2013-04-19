#lang racket/gui
;;; Science Collection
;;; histogram-2d-graphics.rkt
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
;;; -------------------------------------------------------------------
;;;
;;; This module implements the graphics for 2D histograms.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/29/04  Marked as ready for Release 1.0.  (Doug
;;;                    Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changed axes labels.  (Doug Williams)
;;; 4.0.0    08/15/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Upgraded to new plot package. (MDW)

(require plot
         "histogram-2d.rkt")

;;; (histogram-2d-plot h [title]) -> (or/c (is-a?/c image-snip%) void?)
;;;   h :histogram-2d?
;;;   title : string? = "Histogram"
;;; Plot the given 2d histogram using the (new) PLoT collection.
(define (histogram-2d-plot h (title "Histogram"))
  (let* ((nx (histogram-2d-nx h))
         (ny (histogram-2d-nx h))
         (bins (histogram-2d-bins h))
         (x-ranges (histogram-2d-x-ranges h))
         (y-ranges (histogram-2d-y-ranges h))
         (rects
          (for*/list ((ix (in-range nx))
                      (iy (in-range ny)))
            (let ((x0 (vector-ref x-ranges ix))
                  (x1 (vector-ref x-ranges (+ ix 1)))
                  (y0 (vector-ref y-ranges iy))
                  (y1 (vector-ref y-ranges (+ iy 1)))
                  (z (vector-ref bins (+ (* ix ny) iy))))
              (vector (ivl x0 x1) (ivl y0 y1) (ivl 0 z))))))
    (plot3d (rectangles3d rects)
            #:x-min (vector-ref x-ranges 0)
            #:x-max (vector-ref x-ranges (- (vector-length x-ranges) 1))
            #:x-label "x"
            #:y-min (vector-ref y-ranges 0)
            #:y-max (vector-ref y-ranges (- (vector-length y-ranges) 1))
            #:y-label "y"
            #:z-min 0
            #:z-max (histogram-2d-max h)
            #:z-label "Count"
            #:title title)))

;;; (histogram-2d-plot-scaled h [title]) -> (or/c (is-a?/c image-snip%) void?)
;;;   h :histogram-2d?
;;;   title : string? = "Histogram"
;;; Plot the given 2d histogram using the (new) PLoT collection.
(define (histogram-2d-plot-scaled h (title "Histogram"))
  (let* ((nx (histogram-2d-nx h))
         (ny (histogram-2d-nx h))
         (bins (histogram-2d-bins h))
         (x-ranges (histogram-2d-x-ranges h))
         (y-ranges (histogram-2d-y-ranges h))
         (rects
          (for*/list ((ix (in-range nx))
                      (iy (in-range ny)))
            (let ((x0 (vector-ref x-ranges ix))
                  (x1 (vector-ref x-ranges (+ ix 1)))
                  (y0 (vector-ref y-ranges iy))
                  (y1 (vector-ref y-ranges (+ iy 1)))
                  (z (vector-ref bins (+ (* ix ny) iy))))
              (vector (ivl x0 x1) (ivl y0 y1) (ivl 0 z))))))
    (plot3d (rectangles3d rects)
            #:x-min (vector-ref x-ranges 0)
            #:x-max (vector-ref x-ranges (- (vector-length x-ranges) 1))
            #:x-label "x"
            #:y-min (vector-ref y-ranges 0)
            #:y-max (vector-ref y-ranges (- (vector-length y-ranges) 1))
            #:y-label "y"
            #:z-min 0
            #:z-max (histogram-2d-sum h)
            #:z-label "Count"
            #:title title)))

(provide/contract
 (histogram-2d-plot
  (->* (histogram-2d?) (string?) (or/c (is-a?/c image-snip%) void?)))
 (histogram-2d-plot-scaled
  (->* (histogram-2d?) (string?) (or/c (is-a?/c image-snip%) void?))))
