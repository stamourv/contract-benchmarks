#lang racket/base

;; Procedures that plot 2D renderers.

(require "../draw/main.rkt" racket/contract racket/list racket/class racket/match
         unstable/contract
         "../pict.rkt"
         unstable/parameter-group
         racket/lazy-require
         unstable/latent-contract/defthing
         "../common/contract.rkt"
         "../common/math.rkt"
         "../common/draw.rkt"
         "../common/parameters.rkt"
         "../common/plot-element.rkt"
         "../common/file-type.rkt"
         "../common/deprecation-warning.rkt"
         "../common/format.rkt"
         "plot-area.rkt")

;; Require lazily: without this, Racket complains while generating documentation:
;;   cannot instantiate `racket/gui/base' a second time in the same process
(lazy-require ["snip.rkt" (make-2d-plot-snip)]
              ["../common/gui.rkt" (make-snip-frame with-new-eventspace)])

(provide (except-out (all-defined-out) get-renderer-list get-bounds-rect get-ticks plot-dc))

;; ===================================================================================================
;; Plot to a given device context

(define (get-renderer-list renderer-tree)
  (for/list ([r  (flatten (list renderer-tree))])
    (match r
      [(nonrenderer bounds-rect bounds-fun ticks-fun)
       (renderer2d bounds-rect bounds-fun ticks-fun #f)]
      [_  r])))

(define (get-bounds-rect renderer-list x-min x-max y-min y-max)
  (define given-bounds-rect (vector (ivl x-min x-max) (ivl y-min y-max)))
  (define plot-bounds-rect (bounds-fixpoint renderer-list given-bounds-rect))
  (when (or (not (rect-rational? plot-bounds-rect))
            (rect-zero-area? plot-bounds-rect))
    (match-define (vector x-ivl y-ivl) plot-bounds-rect)
    (error 'plot "could not determine sensible plot bounds; got x ∈ ~a, y ∈ ~a"
           (ivl->plot-label x-ivl) (ivl->plot-label y-ivl)))
  (rect-inexact->exact plot-bounds-rect))

(define (get-ticks renderer-list bounds-rect)
  (define-values (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks)
    (for/lists (all-x-ticks all-x-far-ticks all-y-ticks all-y-far-ticks
                            ) ([r  (in-list renderer-list)])
      (define ticks-fun (plot-element-ticks-fun r))
      (cond [ticks-fun  (ticks-fun bounds-rect)]
            [else       (values empty empty empty empty)])))
  (values (remove-duplicates (append* all-x-ticks))
          (remove-duplicates (append* all-x-far-ticks))
          (remove-duplicates (append* all-y-ticks))
          (remove-duplicates (append* all-y-far-ticks))))

(define (plot-dc renderer-list bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks
                 dc x y width height)
  (define area (make-object 2d-plot-area%
                 bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks dc x y width height))
  (send area start-plot)
  
  (define legend-entries
    (flatten (for/list ([rend  (in-list renderer-list)])
               (match-define (renderer2d rend-bounds-rect _bf _tf render-proc) rend)
               (send area start-renderer (if rend-bounds-rect
                                             (rect-inexact->exact rend-bounds-rect)
                                             (unknown-rect 2)))
               (if render-proc (render-proc area) empty))))
  
  (send area end-renderers)
  
  (when (not (empty? legend-entries))
    (send area draw-legend legend-entries))
  
  (send area end-plot))

(defproc (plot/dc [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                  [dc (is-a?/c dc<%>)]
                  [x real?] [y real?] [width (>=/c 0)] [height (>=/c 0)]
                  [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                  [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                  [#:title title (or/c string? #f) (plot-title)]
                  [#:x-label x-label (or/c string? #f) (plot-x-label)]
                  [#:y-label y-label (or/c string? #f) (plot-y-label)]
                  [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define renderer-list (get-renderer-list renderer-tree))
  (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max))
  (define-values (x-ticks x-far-ticks y-ticks y-far-ticks)
    (get-ticks renderer-list bounds-rect))
  
  (parameterize ([plot-title          title]
                 [plot-x-label        x-label]
                 [plot-y-label        y-label]
                 [plot-legend-anchor  legend-anchor])
    (plot-dc renderer-list bounds-rect x-ticks x-far-ticks y-ticks y-far-ticks
             dc x y width height)))

;; ===================================================================================================
;; Plot to various other backends

;; Plot to a bitmap
(defproc (plot-bitmap [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                      [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                      [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                      [#:width width exact-positive-integer? (plot-width)]
                      [#:height height exact-positive-integer? (plot-height)]
                      [#:title title (or/c string? #f) (plot-title)]
                      [#:x-label x-label (or/c string? #f) (plot-x-label)]
                      [#:y-label y-label (or/c string? #f) (plot-y-label)]
                      [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                      ) (is-a?/c bitmap%)
  (define renderer-list (get-renderer-list renderer-tree))
  (define bounds-rect (get-bounds-rect renderer-list x-min x-max y-min y-max))
  (define-values (x-ticks x-far-ticks y-ticks y-far-ticks)
    (get-ticks renderer-list bounds-rect))
  ((if (plot-animating?) draw-bitmap draw-bitmap/supersampling)
   (λ (dc) 
     (plot/dc renderer-tree dc 0 0 width height
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
   width height))

(defproc (plot-pict [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                    [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                    [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                    [#:width width exact-positive-integer? (plot-width)]
                    [#:height height exact-positive-integer? (plot-height)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]
                    ) pict?
  (define saved-values (plot-parameters))
  (dc (λ (dc x y)
        (parameterize/group
            ([plot-parameters  saved-values])
          (plot/dc renderer-tree dc x y width height
                   #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                   #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)))
      width height))

;; Plot to a file
(defproc (plot-file [renderer-tree (treeof (or/c renderer2d? nonrenderer?))]
                    [output (or/c path-string? output-port?)]
                    [kind (one-of/c 'auto 'png 'jpeg 'xmb 'xpm 'bmp 'ps 'pdf 'svg) 'auto]
                    [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                    [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                    [#:width width exact-positive-integer? (plot-width)]
                    [#:height height exact-positive-integer? (plot-height)]
                    [#:title title (or/c string? #f) (plot-title)]
                    [#:x-label x-label (or/c string? #f) (plot-x-label)]
                    [#:y-label y-label (or/c string? #f) (plot-y-label)]
                    [#:legend-anchor legend-anchor anchor/c (plot-legend-anchor)]) void?
  (define real-kind (if (eq? kind 'auto) (detect-image-file-type output) kind))
  (case real-kind
    [(png jpeg xbm xpm bmp)
     (define bm
       (plot-bitmap
        renderer-tree
        #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max #:width width #:height height
        #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor))
     (send bm save-file output real-kind (plot-jpeg-quality))]
    [(ps pdf svg)
     (define dc
       (case real-kind
         [(ps)  (new post-script-dc%
                     [interactive (plot-ps/pdf-interactive?)] [parent #f] [use-paper-bbox #f]
                     [as-eps #t] [width width] [height height] [output output])]
         [(pdf)  (new pdf-dc%
                      [interactive (plot-ps/pdf-interactive?)] [parent #f] [use-paper-bbox #f]
                      [width width] [height height] [output output])]
         [(svg)  (new svg-dc%
                      [width width] [height height] [output output] [exists 'truncate/replace])]))
     (define-values (x-scale y-scale) (send dc get-device-scale))
     (send dc start-doc "Rendering plot")
     (send dc start-page)
     (plot/dc renderer-tree dc 0 0
              (inexact->exact (/ width x-scale)) (inexact->exact (/ height y-scale))
              #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
              #:title title #:x-label x-label #:y-label y-label #:legend-anchor legend-anchor)
     (send dc end-page)
     (send dc end-doc)])
  (void))

