#lang racket/gui
;;; Science Collection
;;; discrete-histogram-graphics.rkt
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
;;; -----------------------------------------------------------------------------
;;;
;;; This module implements the graphics for discrete histograms.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/30/04  Marked as ready for Release 1.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0 (MDW)
;;; 3.0.1    07/01/08  Changed x axis label. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)
;;; 4.1.0    10/09/11  Upgraded to new plot package. (MDW)

(require plot
         "discrete-histogram.rkt")

;;; (discrete-histogram-plot h [title]) -> (or/c (is-a?/c image-snip%) void?)
;;;   h : discrete-histogram?
;;;   title : string? = "Histogram"
;;; Plot the given discrete histogram using the (new) PLoT collection.
;(define (discrete-histogram-plot h (title "Histogram"))
;  (let* ((n1 (discrete-histogram-n1 h))
;         (n2 (discrete-histogram-n2 h))
;         (bins (discrete-histogram-bins h))
;         (cat-vals
;          (for/list ((i (in-range n1 (+ n2 1)))
;                     (j (in-naturals)))
;            (vector i (vector-ref bins j)))))
;    (plot (discrete-histogram cat-vals)
;          #:x-label "x"
;          #:y-min 0 #:y-max (discrete-histogram-max h)
;          #:y-label "Count"
;          #:title title)))
(define (discrete-histogram-plot h (title "Histogram"))
  (let ((n1 (discrete-histogram-n1 h))
        (n2 (discrete-histogram-n2 h))
        (bins (discrete-histogram-bins h)))
    (if (< (- n2 n1) 20)
        (let ((cat-vals
               (for/list ((i (in-range n1 (+ n2 1)))
                          (j (in-naturals)))
                 (vector i (vector-ref bins j)))))
          (plot (discrete-histogram cat-vals)
                #:x-label "x"
                #:y-min 0 #:y-max (discrete-histogram-max h)
                #:y-label "Count"
                #:title title))
        (let ((rects
               (for/list ((i (in-range n1 (+ n2 1)))
                          (j (in-naturals)))
                 (vector (ivl (- i 1/2) (+ i 1/2))
                         (ivl 0 (vector-ref bins j))))))
          (plot (rectangles rects)
                #:x-min (- n1 1)
                #:x-max (+ n2 1)
                #:x-label "x"
                #:y-min 0 #:y-max (discrete-histogram-max h)
                #:y-label "Count"
                #:title title)))))

;;; (discrete-histogram-plot-scaled h [title]) -> (or/c (is-a?/c image-snip%) void?)
;;;   h : discrete-histogram?
;;;   title : string? = "Histogram"
;;; Plot the given discrete histogram using the (new) PLoT collection.
;(define (discrete-histogram-plot-scaled h (title "Histogram"))
;  (let* ((n1 (discrete-histogram-n1 h))
;         (n2 (discrete-histogram-n2 h))
;         (bins (discrete-histogram-bins h))
;         (cat-vals
;          (for/list ((i (in-range n1 (+ n2 1)))
;                     (j (in-naturals)))
;            (vector i (vector-ref bins j)))))
;    (plot (discrete-histogram cat-vals)
;          #:x-label "x"
;          #:y-min 0 #:y-max (discrete-histogram-sum h)
;          #:y-label "Count"
;          #:title title)))
(define (discrete-histogram-plot-scaled h (title "Scaled Histogram"))
  (let ((n1 (discrete-histogram-n1 h))
        (n2 (discrete-histogram-n2 h))
        (bins (discrete-histogram-bins h)))
    (if (< (- n2 n1) 20)
        (let ((cat-vals
               (for/list ((i (in-range n1 (+ n2 1)))
                          (j (in-naturals)))
                 (vector i (vector-ref bins j)))))
          (plot (discrete-histogram cat-vals)
                #:x-label "x"
                #:y-min 0 #:y-max (discrete-histogram-max h)
                #:y-label "Count"
                #:title title))
        (let ((rects
               (for/list ((i (in-range n1 (+ n2 1)))
                          (j (in-naturals)))
                 (vector (ivl (- i 1/2) (+ i 1/2))
                         (ivl 0 (vector-ref bins j))))))
          (plot (rectangles rects)
                #:x-min (- n1 1)
                #:x-max (+ n2 1)
                #:x-label "x"
                #:y-min 0 #:y-max (discrete-histogram-sum h)
                #:y-label "Count"
                #:title title)))))

;;; Module Contracts

(provide/contract
 (discrete-histogram-plot
  (->* (discrete-histogram?) (string?) (or/c (is-a?/c image-snip%) void?)))
 (discrete-histogram-plot-scaled
  (->* (discrete-histogram?) (string?) (or/c (is-a?/c image-snip%) void?))))
