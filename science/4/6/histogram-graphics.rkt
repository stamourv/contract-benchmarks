#lang racket/gui
;;; Science Collection
;;; histogram-graphics.rkt
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
;;; This module provides graphics for histograms.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/28/04  Added contracts for functions.  Marked as ready
;;;                    for Release 1.0.  (Doug Williams)
;;; 3.0.0    06/09/08  Changes required for V4.0.  (Doug Williams)
;;; 3.0.1    07/01/08  Changed x axis label.  (Doug Williams)
;;; 4.0.0    08/16/11  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Upgraded to new plot package. (MDW)

(require plot 
         "histogram.rkt")

;;; (histogram-plot h [title]) -> (or/c (is-a?/c image-snip%) void?)
;;;   h :histogram?
;;;   title : string? = "Histogram"
;;; Plot the given histogram using the (new) PLoT collection.
(define (histogram-plot h (title "Histogram"))
  (let* ((n (histogram-n h))
         (bins (histogram-bins h))
         (ranges (histogram-ranges h))
         (rects
          (for/list ((i (in-range n)))
           (let ((x0 (vector-ref ranges i))
                 (x1 (vector-ref ranges (+ i 1)))
                 (y (vector-ref bins i)))
             (vector (ivl x0 x1) (ivl 0 y))))))
    (plot (rectangles rects)
          #:x-min (vector-ref ranges 0)
          #:x-max (vector-ref ranges (- (vector-length ranges) 1))
          #:x-label "x"
          #:y-min 0
          #:y-max (histogram-max h)
          #:y-label "Count"
          #:title title)))

;;; (histogram-plot-scaled h [title]) -> (or/c (is-a?/c image-snip%) void?)
;;;   h :histogram?
;;;   title : string? = "Histogram"
;;; Plot the given histogram using the (new) PLoT collection.
(define (histogram-plot-scaled h (title "Histogram"))
  (let* ((n (histogram-n h))
         (bins (histogram-bins h))
         (ranges (histogram-ranges h))
         (rects
          (for/list ((i (in-range n)))
           (let ((x0 (vector-ref ranges i))
                 (x1 (vector-ref ranges (+ i 1)))
                 (y (vector-ref bins i)))
             (vector (ivl x0 x1) (ivl 0 y))))))
    (plot (rectangles rects)
          #:x-min (vector-ref ranges 0)
          #:x-max (vector-ref ranges (- (vector-length ranges) 1))
          #:x-label "x"
          #:y-min 0
          #:y-max (histogram-sum h)
          #:y-label "Count"
          #:title title)))

;;; Module Contracts

(provide/contract
 (histogram-plot
  (->* (histogram?) (string?) (or/c (is-a?/c image-snip%) void?)))
 (histogram-plot-scaled
  (->* (histogram?) (string?) (or/c (is-a?/c image-snip%) void?))))
