#lang slideshow
(require "movie.rkt"
         "analogy.rkt"
         "plt.rkt"
         "end.rkt"
         "peek.rkt")

(slide (blank))
(time
 (begin
   (movie-slides (slide->pict (retract-most-recent-slide)))
   (analogy-slides)
   (plt-slides)
   (end-slides)
   (peek-slides final-end-slide)))
(exit)
