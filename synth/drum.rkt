#lang racket

(require math/array)

(require "synth.rkt")

(provide drum)

(define (random-sample) (- (* 2.0 (random)) 1.0))

;; Drum "samples" (Arrays of floats)
;; TODO compute those at compile-time
(define bass-drum
  (let ()
    ;; 0.05 seconds of noise whose value changes every 12 samples
    (define n-samples           (seconds->samples 0.05))
    (define n-different-samples (quotient n-samples 12))
    (for/array #:shape (vector n-samples) #:fill 0.0
               ([i      (in-range n-different-samples)]
                [sample (in-producer random-sample (lambda _ #f))]
                #:when #t
                [j (in-range 12)])
      sample)))
(define snare
  ;; 0.05 seconds of noise
  (build-array (vector (seconds->samples 0.05))
               (lambda (x) (random-sample))))

;; limited drum machine
;; drum patterns are simply lists with either O (bass drum), X (snare) or
;; #f (pause)
(define (drum n pattern tempo)
  (define samples-per-beat (quotient (* fs 60) tempo))
  (define (make-drum drum-sample samples-per-beat)
    (array-append*
     (list drum-sample
           (make-array (vector (- samples-per-beat
                                  (array-size drum-sample)))
                       0.0))))
  (define O     (make-drum bass-drum samples-per-beat))
  (define X     (make-drum snare     samples-per-beat))
  (define pause (make-array (vector samples-per-beat) 0.0))
  (array-append*
   (for*/list ([i    (in-range n)]
               [beat (in-list pattern)])
     (case beat
       ((X)  X)
       ((O)  O)
       ((#f) pause)))))
;; TODO more drums, cymbals, etc.
