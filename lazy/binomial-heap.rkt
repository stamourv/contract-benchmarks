#lang racket/base

(provide empty-heap
         make-binomial-heap-ops)

(define empty-heap #f)

(define (make-binomial-heap-ops kons hd tl rev
                                make-node node-rank node-val node-obj node-children)
  (define (link t1 t2)
    (define r (node-rank t1))
    (define x1 (node-val t1))
    (define x2 (node-val t2))
    (cond
      [(<= x1 x2)
       (define c1 (node-children t1))
       (define o1 (node-obj t1))
       (make-node (+ r 1) x1 o1 (kons t2 c1))]
      [else
       (define c2 (node-children t2))
       (define o2 (node-obj t2))
       (make-node (+ r 1) x2 o2 (kons t1 c2))]))
  (define (ins-tree t ts)
    (cond
      [(not ts) (kons t #f)]
      [else
       (define tt (hd ts))
       (if (< (node-rank t) (node-rank tt))
           (kons t ts)
           (ins-tree (link t tt) (tl ts)))]))
  
  (define (insert x obj ts)
    (ins-tree (make-node 0 x obj #f) ts))

  (define (merge ts1 ts2)
    (cond
      [(not ts2) ts1]
      [(not ts1) ts2]
      [else 
       (define t1 (hd ts1))
       (define t2 (hd ts2))
       (cond
         [(< (node-rank t1) (node-rank t2))
          (define tts1 (tl ts1))
          (kons t1 (merge tts1 ts2))]
         [(< (node-rank t2) (node-rank t1))
          (define tts2 (tl ts2))
          (kons t2 (merge ts1 tts2))]
         [else
          (define tts1 (tl ts1))
          (define tts2 (tl ts2))
          (ins-tree (link t1 t2) (merge tts1 tts2))])]))
  
  (define (remove-min-tree l)
    (cond
      [(not l) (error 'empty)]
      [(not (tl l)) (values (hd l) #f)]
      [else 
       (define t (hd l))
       (define ts (tl l))
       (define-values (tt tts) (remove-min-tree ts))
       (if (<= (node-val t) (node-val tt))
           (values t ts)
           (values tt (kons t tts)))]))
  
  (define (find-min ts)
    (define-values (t tts) (remove-min-tree ts))
    t)
  (define (find-min-priority ts)
    (node-val (find-min ts)))
  (define (find-min-obj ts) (node-obj (find-min ts)))
  (define (delete-min ts)
    (define-values (t tts) (remove-min-tree ts))
    (merge (rev (node-children t)) tts))
  
  (values insert delete-min find-min-priority find-min-obj))
