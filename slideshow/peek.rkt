#lang slideshow
(require "princess.rkt"
         "movie.rkt"
         slideshow/play
         scheme/math)

(provide peek-slides)

(define p-width (pict-width (scale (make-princess) 2.0)))

(define (peek-slides final-end-slide)
  (play-it
   #:name "Wave"
   '([10 0.05]
     [40 0.05])
   (lambda (peek-n wave-n)
     (define (modulate n)
       (let loop ([n n])
         (if (n . > . 2.0)
             (loop (- n 2.0))
             (if (< n 1.0)
                 n
                 (- 2.0 n)))))
     (clip-to-screen
      (pin-over
       (cc-superimpose full-page
                       final-end-slide)
       (+ (- (* p-width 1.5)) (* p-width 1.7 peek-n))
       0
       (scale
        (make-princess #:rotate (* peek-n (/ pi -5))
                       #:front-arm-angle (+ (* pi 4/10)
                                            (* (* pi 2/10) (sin (* (modulate (* 4 wave-n)) pi)))))
        2.0))))))
