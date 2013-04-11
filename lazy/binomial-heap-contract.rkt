#lang racket/base 
(require racket/contract
         "binomial-heap.rkt")

(provide binomial-heap?
         binomial-heap/c
         binomial-heap/opt/c
         
         insert remove-min find-min-priority find-min-obj
         
         c:insert c:remove-min c:find-min-priority c:find-min-obj
         o:insert o:remove-min o:find-min-priority o:find-min-obj
         ?:insert ?:remove-min ?:find-min-priority ?:find-min-obj
         h:insert h:remove-min h:find-min-priority h:find-min-obj
         s:insert s:remove-min s:find-min-priority s:find-min-obj
         t:insert t:remove-min t:find-min-priority t:find-min-obj
         n:insert n:remove-min n:find-min-priority n:find-min-obj
         
         make-node
         node-rank
         node-val
         node-obj
         node-children
         make-kons
         kons-hd
         kons-tl
         
         make-h:node
         h:node-rank
         h:node-val
         h:node-obj
         h:node-children
         make-h:kons
         h:kons-hd
         h:kons-tl)

(define-contract-struct kons (hd tl) (make-inspector))
(define-contract-struct node (rank val obj children) (make-inspector))

(define (kfoldl f init kl res)
  (cond
    [(not kl) res]
    [(not res) (kfoldl f init (kons-tl kl) (f (kons-hd kl) init))]
    [else (kfoldl f init (kons-tl kl) (f (kons-hd kl) res))]))
(define (rev kl)
  (kfoldl make-kons #f kl #f))


(define-struct h:kons (hd tl) #:transparent)
(define-struct h:node (rank val obj children) #:transparent)
(define (h:kfoldl f init kl res)
  (cond
    [(not kl) res]
    [(not res) (h:kfoldl f init (h:kons-tl kl) (f (h:kons-hd kl) init))]
    [else (h:kfoldl f init (h:kons-tl kl) (f (h:kons-hd kl) res))]))
(define (h:rev kl)
  (h:kfoldl make-h:kons #f kl #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; normal
;;

(define (binomial-tree-rank=/c r v)
  (or/c #f
        (node/dc [rank (=/c r)]
                 [val (>=/c v)]
                 [obj any/c]
                 [children (rank val) (heap-ordered/desc (- rank 1) val)])))
(define (binomial-tree-rank>/c r)
  (or/c #f
        (node/dc [rank (>=/c r)]
                 [val any/c]
                 [obj any/c]
                 [children (rank val) (heap-ordered/desc (- rank 1) val)])))
(define (heap-ordered/desc rank val)
  (or/c #f
        (kons/dc [hd (binomial-tree-rank=/c rank val)]
                 [tl () (heap-ordered/desc (- rank 1) val)])))
(define (binomial-trees/asc rank)
  (or/c #f
        (kons/dc [hd (binomial-tree-rank>/c rank)]
                 [tl (hd) (binomial-trees/asc (node-rank hd))])))

(define binomial-heap/c (binomial-trees/asc -inf.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; opt
;;

(define-opt/c (binomial-tree-rank=/opt/c r v)
  (or/c #f
        (node/dc [rank (=/c r)]
                 [val (>=/c v)]
                 [obj any/c]
                 [children (rank val) (heap-ordered/desc/opt (- rank 1) val)])))
(define-opt/c (binomial-tree-rank>/opt/c r)
  (or/c #f
        (node/dc [rank (>=/c r)]
                 [val any/c]
                 [obj any/c]
                 [children (rank val) (heap-ordered/desc/opt (- rank 1) val)])))
(define-opt/c (heap-ordered/desc/opt rank val)
  (or/c #f
        (kons/dc [hd (binomial-tree-rank=/opt/c rank val)]
                 [tl () (heap-ordered/desc/opt (- rank 1) val)])))
(define-opt/c (binomial-trees/asc/opt rank)
  (or/c #f
        (kons/dc [hd (binomial-tree-rank>/opt/c rank)]
                 [tl (hd) (binomial-trees/asc/opt (node-rank hd))])))

(define binomial-heap/opt/c (binomial-trees/asc/opt -inf.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; flat
;;

(define (binomial-heap? btree) (binomial-trees/asc? -inf.0 btree))

(define (binomial-tree-rank=? r v btree)
  (or (not btree)
      (and (node? btree)
           (= (node-rank btree) r)
           (>= (node-val btree) v)
           (heap-ordered/desc? (- (node-rank btree) 1) (node-val btree) (node-children btree)))))

(define (binomial-tree-rank>? r btree)
  (or (not btree)
      (and (node? btree)
           (>= (node-rank btree) r)
           (heap-ordered/desc? (- (node-rank btree) 1) (node-val btree) (node-children btree)))))

(define (heap-ordered/desc? rank val vals)
  (or (not vals)
      (and (kons? vals)
           (binomial-tree-rank=? rank val (kons-hd vals))
           (heap-ordered/desc? (- rank 1) val (kons-tl vals)))))

(define (binomial-trees/asc? rank vals)
  (or (not vals)
      (and (kons? vals)
           (binomial-tree-rank>? rank (kons-hd vals))
           (binomial-trees/asc? (node-rank (kons-hd vals)) (kons-tl vals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; hand-rolled chaperone
;;

(define (binomial-tree-rank=/chap-proj r v blame val)
  (cond
    [(not val) val]
    [(h:node? val) 
     (define vals-rank (h:node-rank val))
     (define vals-val (h:node-val val))
     (unless (and (number? vals-rank) (= vals-rank r))
       (raise-blame-error blame val "expected a node with rank ~e" r))
     (unless (and (number? vals-val) (>= vals-val v))
       (raise-blame-error blame val "expected a node with val >= ~e" v))
     (define c #f)
     (chaperone-struct
      val
      h:node-children 
      (λ (n children) 
        (cond [c c]
              [else
               (set! c (add-heap-ordered/desc/chap-proj (- (h:node-rank n) 1) (h:node-val n) blame children))
               c]))
      binomial-tree-rank=/chap-proj-prop-desc 
      (vector r v))]
    [else
      (raise-blame-error blame v "expected a node or #f")]))

(define-values (binomial-tree-rank=/chap-proj-prop-desc
                binomial-tree-rank=/chap-proj-prop-pred?
                binomial-tree-rank=/chap-proj-prop-get)
  (make-impersonator-property 'binomial-tree-rank=))

(define (add-binomial-tree-rank=/chap-proj r v blame val)
  (if (and (binomial-tree-rank=/chap-proj-prop-pred? val)
           (let ([rv-vec (binomial-tree-rank=/chap-proj-prop-get val)])
             (and (eq? r (vector-ref rv-vec 0))
                  (eq? v (vector-ref rv-vec 0))))) 
      val
      (binomial-tree-rank=/chap-proj r v blame val)))

(define (binomial-tree-rank>/chap-proj r blame v)
  (cond
    [(not v) #f]
    [(h:node? v)
     (define rank (h:node-rank v)) 
     (unless (>= rank r)
       (raise-blame-error blame v "expected a node struct with rank larger than ~e" r))
     (define c #f)
     (chaperone-struct 
      v
      h:node-children
      (λ (s children)
        (cond
          [c c]
          [else
           (set! c (add-heap-ordered/desc/chap-proj (- rank 1) (h:node-val s) blame children))
           c]))
      binomial-tree-rank>/chap-proj-prop-desc
      (vector r))]
    [else (raise-blame-error blame v "expected a node struct or #f")]))

(define-values (binomial-tree-rank>/chap-proj-prop-desc
                binomial-tree-rank>/chap-proj-prop-pred?
                binomial-tree-rank>/chap-proj-prop-get)
  (make-impersonator-property 'binomial-tree-rank>))

(define (add-binomial-tree-rank>/chap-proj r blame v)
  (if (and (binomial-tree-rank>/chap-proj-prop-pred? v)
           (eq? r (vector-ref (binomial-tree-rank>/chap-proj-prop-get v) 0)))
      v
      (binomial-tree-rank>/chap-proj r blame v)))

(define (heap-ordered/desc/chap-proj rank val blame v)
  (cond
    [(not v) #f]
    [(h:kons? v)
     (define h #f)
     (define t #f)
     (chaperone-struct
      v
      h:kons-hd 
      (λ (k hd) 
        (cond
          [h h]
          [else 
           (set! h (add-binomial-tree-rank=/chap-proj rank val blame hd))
           h]))
      h:kons-tl
      (λ (k tl)
        (cond
          [t t]
          [else 
           (set! t (add-heap-ordered/desc/chap-proj (- rank 1) val blame tl))
           t]))
      heap-ordered/desc/chap-proj-prop-desc
      (vector rank val))]
    [else (raise-blame-error blame v "expected #f or a kons")]))

(define-values (heap-ordered/desc/chap-proj-prop-desc
                heap-ordered/desc/chap-proj-prop-pred?
                heap-ordered/desc/chap-proj-prop-get)
  (make-impersonator-property 'heap-ordered/desc))

(define (add-heap-ordered/desc/chap-proj rank val blame v)
  (if (and (heap-ordered/desc/chap-proj-prop-pred? v)
           (let ([rank-val-vec (heap-ordered/desc/chap-proj-prop-get v)])
             (and (eq? (vector-ref rank-val-vec 0) rank)
                  (eq? (vector-ref rank-val-vec 1) val))))
      v
      (heap-ordered/desc/chap-proj rank val blame v)))
  
(define (binomial-trees/asc/chap-proj rank blame v)
  (cond
    [(not v) #f]
    [(h:kons? v)
     (define h #f)
     (define t #f)
     (chaperone-struct
      v
      h:kons-hd 
      (λ (k hd) 
        (cond
          [h h]
          [else 
           (set! h (add-binomial-tree-rank>/chap-proj rank blame hd))
           h]))
      h:kons-tl
      (λ (k tl)
        (cond
          [t t]
          [else 
           (set! t (add-binomial-trees/asc/chap-proj (h:node-rank (h:kons-hd k)) blame tl))
           t]))
      binomial-trees/asc/chap-proj-prop-desc
      (vector rank))]
    [else (raise-blame-error blame v "expected #f or a kons")]))

(define-values (binomial-trees/asc/chap-proj-prop-desc
                binomial-trees/asc/chap-proj-prop-pred?
                binomial-trees/asc/chap-proj-prop-get)
  (make-impersonator-property 'heap-ordered/desc))

(define (add-binomial-trees/asc/chap-proj rank blame v)
  (if (and (binomial-trees/asc/chap-proj-prop-pred? v)
           (eq? (vector-ref (binomial-trees/asc/chap-proj-prop-get v) 0) rank))
      v
      (binomial-trees/asc/chap-proj rank blame v)))

(define-struct binomial-tree-chap-contract-struct ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection (λ (ctc) (λ (blame) (λ (v) (add-binomial-trees/asc/chap-proj -inf.0 blame v))))
   #:name (λ (ctc) 'binomial-tree-chap-contract)
   #:first-order node?
   #:stronger (λ (this that) #f)))

(define binomial-heap/chap (binomial-tree-chap-contract-struct))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; struct/c chaperone
;;


(define (binomial-tree-rank=/sc r v)
  (or/c #f
        (struct/dc h:node
                   [rank (=/c r)]
                   [val (>=/c v)]
                   [children (rank val) #:lazy (heap-ordered/desc/sc (- rank 1) val)])))
(define (binomial-tree-rank>/sc r)
  (or/c #f
        (struct/dc h:node
                   [rank (>=/c r)]
                   [val any/c]
                   [children (rank val) #:lazy (heap-ordered/desc/sc (- rank 1) val)])))
(define (heap-ordered/desc/sc rank val)
  (or/c #f
        (struct/dc h:kons
                   [hd #:lazy (binomial-tree-rank=/sc rank val)]
                   [tl () #:lazy (heap-ordered/desc/sc (- rank 1) val)])))
(define (binomial-trees/asc/sc rank)
  (or/c #f
        (struct/dc h:kons
                   [hd #:lazy (binomial-tree-rank>/sc rank)]
                   [tl (hd) #:lazy (binomial-trees/asc/sc (h:node-rank hd))])))

(define binomial-heap/sc (binomial-trees/asc/sc -inf.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; struct/c chaperone opt
;;


(define-opt/c (binomial-tree-rank=/sco r v)
  (or/c #f
        (struct/dc h:node
                   [rank (=/c r)]
                   [val (>=/c v)]
                   [children (rank val) #:lazy (heap-ordered/desc/sco (- rank 1) val)])))
(define-opt/c (binomial-tree-rank>/sco r)
  (or/c #f
        (struct/dc h:node
                   [rank (>=/c r)]
                   [val any/c]
                   [children (rank val) #:lazy (heap-ordered/desc/sco (- rank 1) val)])))
(define-opt/c (heap-ordered/desc/sco rank val)
  (or/c #f
        (struct/dc h:kons
                   [hd #:lazy (binomial-tree-rank=/sco rank val)]
                   [tl () #:lazy (heap-ordered/desc/sco (- rank 1) val)])))
(define-opt/c (binomial-trees/asc/sco rank)
  (or/c #f
        (struct/dc h:kons
                   [hd #:lazy (binomial-tree-rank>/sco rank)]
                   [tl (hd) #:lazy (binomial-trees/asc/sco (h:node-rank hd))])))

(define binomial-heap/sco (binomial-trees/asc/sco -inf.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; struct/c, cons chaperone opt
;;
;; this doesn't work because the cons/c aren't lazy
;;


(define-opt/c (binomial-tree-rank=/scon r v)
  (or/c #f
        (struct/dc h:node
                   [rank (=/c r)]
                   [val (>=/c v)]
                   [children (rank val) #:lazy (heap-ordered/desc/scon (- rank 1) val)])))
(define-opt/c (binomial-tree-rank>/scon r)
  (or/c #f
        (struct/dc h:node
                   [rank (>=/c r)]
                   [val any/c]
                   [children (rank val) #:lazy (heap-ordered/desc/scon (- rank 1) val)])))
(define-opt/c (heap-ordered/desc/scon rank val)
  (or/c #f
        (cons/c (binomial-tree-rank=/scon rank val)
                (heap-ordered/desc/scon (- rank 1) val))))
(define-opt/c (binomial-trees/asc/scon rank)
  (or/c #f
        (cons/c (binomial-tree-rank>/scon rank)
                (binomial-trees/asc/scon -inf.0)))) ;; this contract is wrong

(define binomial-heap/scon (binomial-trees/asc/scon -inf.0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; create the variously contracted operations.
;;

(define-values (insert remove-min find-min-priority find-min-obj)
  (make-binomial-heap-ops make-kons kons-hd kons-tl rev
                          make-node node-rank node-val node-obj node-children))
(define-values (h:insert/r h:remove-min/r h:find-min-priority/r h:find-min-obj/r)
  (make-binomial-heap-ops make-h:kons h:kons-hd h:kons-tl h:rev
                          make-h:node h:node-rank h:node-val h:node-obj h:node-children))
(define-values (n:insert/r n:remove-min/r n:find-min-priority/r n:find-min-obj/r)
  (make-binomial-heap-ops cons car cdr reverse
                          make-h:node h:node-rank h:node-val h:node-obj h:node-children))

(define (apply-contract c v) (contract c v 'pos 'neg))

(define c:insert (apply-contract (-> number? any/c binomial-heap/c binomial-heap/c) insert))
(define c:find-min-obj (apply-contract (-> binomial-heap/c number?) find-min-obj))
(define c:find-min-priority (apply-contract (-> binomial-heap/c number?) find-min-priority))
(define c:remove-min (apply-contract (-> binomial-heap/c binomial-heap/c) remove-min))

(define o:insert (apply-contract (-> number? any/c binomial-heap/opt/c binomial-heap/opt/c) insert))
(define o:find-min-obj (apply-contract (-> binomial-heap/opt/c number?) find-min-obj))
(define o:find-min-priority (apply-contract (-> binomial-heap/opt/c number?) find-min-priority))
(define o:remove-min (apply-contract (-> binomial-heap/opt/c binomial-heap/opt/c) remove-min))

(define ?:insert (apply-contract (-> number? any/c binomial-heap? binomial-heap?) insert))
(define ?:find-min-obj (apply-contract (-> binomial-heap? number?) find-min-obj))
(define ?:find-min-priority (apply-contract (-> binomial-heap? number?) find-min-priority))
(define ?:remove-min (apply-contract (-> binomial-heap? binomial-heap?) remove-min))

(define h:insert (apply-contract (-> number? any/c binomial-heap/chap binomial-heap/chap) h:insert/r))
(define h:find-min-obj (apply-contract (-> binomial-heap/chap number?) h:find-min-obj/r))
(define h:find-min-priority (apply-contract (-> binomial-heap/chap number?) h:find-min-priority/r))
(define h:remove-min (apply-contract (-> binomial-heap/chap binomial-heap/chap) h:remove-min/r))

(define s:insert (apply-contract (-> number? any/c binomial-heap/sc binomial-heap/sc) h:insert/r))
(define s:find-min-obj (apply-contract (-> binomial-heap/sc number?) h:find-min-obj/r))
(define s:find-min-priority (apply-contract (-> binomial-heap/sc number?) h:find-min-priority/r))
(define s:remove-min (apply-contract (-> binomial-heap/sc binomial-heap/sc) h:remove-min/r))

(define t:insert (apply-contract (-> number? any/c binomial-heap/sco binomial-heap/sco) h:insert/r))
(define t:find-min-obj (apply-contract (-> binomial-heap/sco number?) h:find-min-obj/r))
(define t:find-min-priority (apply-contract (-> binomial-heap/sco number?) h:find-min-priority/r))
(define t:remove-min (apply-contract (-> binomial-heap/sco binomial-heap/sco) h:remove-min/r))

(define n:insert (apply-contract (-> number? any/c binomial-heap/scon binomial-heap/scon) h:insert/r))
(define n:find-min-obj (apply-contract (-> binomial-heap/scon number?) h:find-min-obj/r))
(define n:find-min-priority (apply-contract (-> binomial-heap/scon number?) h:find-min-priority/r))
(define n:remove-min (apply-contract (-> binomial-heap/scon binomial-heap/scon) h:remove-min/r))

