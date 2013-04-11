#lang racket/base

(require "binomial-heap.rkt"
         "binomial-heap-contract.rkt"
         racket/contract
         (for-syntax racket/base))

(define tests 0)
(define failing-tests 0)

(define show-running-test-cases? #f)

(define-syntax (tst stx)
  (syntax-case stx ()
    [(_ x y)
     (with-syntax ([l (syntax-line stx)])
       #'(tst/proc (λ () x) y l))]))

(define-syntax (tst/x stx)
  (syntax-case stx ()
    [(_ x)
     (with-syntax ([l (syntax-line stx)])
       #'(tst/exn (λ () x) l))]))

(define (tst/exn x l)
  (set! tests (+ tests 1))
  (when show-running-test-cases? (printf "running  test on line ~a\n" l))
  (with-handlers ((exn:fail:contract:blame? void))
    (define ans (x))
    (set! failing-tests (+ failing-tests 1))
    (fprintf (current-error-port)
             "test on line ~s was supposed to raise an exception, but did not, got ~s\n" 
             l
             ans))
  (when show-running-test-cases? (printf "finished test on line ~a\n" l)))

(define (tst/proc x y l)
  (set! tests (+ tests 1))
  (when show-running-test-cases? (printf "running  test on line ~a\n" l))
  (let ([got (with-handlers ((exn:fail? values)) (x))])
    (unless (equal? got y)
      (set! failing-tests (+ failing-tests 1))
      (cond
        [(exn:fail? got)
         (eprintf "test on line ~s: expected ~s, but got:\n" l y)
         (define sp (open-output-string))
         (parameterize ([current-error-port sp])
           ((error-display-handler) (exn-message got) got))
         (display (regexp-replace* #rx"\n(.)" (string-append "  " (get-output-string sp)) "\n  \\1"))]
        [else
         (eprintf "test on line ~s: ~s, but expected ~s\n"
                  l got y)])))
  (when show-running-test-cases? (printf "finished test on line ~a\n" l)))

(let ([th (foldl (λ (x y) (insert x x y)) empty-heap (list 9 -123 59 85 52 2 0 -3 19283401924 59235))])
  (begin
    (tst (find-min-priority th) -123)
    (tst (find-min-obj th) -123)
    (tst (find-min-priority (remove-min th)) -3)
    (tst (find-min-obj (remove-min th)) -3)
    (tst (find-min-priority (remove-min (remove-min th))) 0)
    (tst (find-min-obj (remove-min (remove-min th))) 0)
    (tst (remove-min (insert 0 0 empty-heap)) empty-heap)
    (tst (insert 0 0 (remove-min (insert 0 0 empty-heap))) (make-kons (make-node 0 0 0 #f) #f))))

(let ([th (foldl (λ (x y) (c:insert x x y)) empty-heap (list 9 -123 59 85 52 2 0 -3 19283401924 59235))])
  (begin
    (tst (c:find-min-priority th) -123)
    (tst (c:find-min-obj th) -123)
    (tst (c:find-min-priority (c:remove-min th)) -3)
    (tst (c:find-min-obj (c:remove-min th)) -3)
    (tst (c:find-min-priority (c:remove-min (c:remove-min th))) 0)
    (tst (c:find-min-obj (c:remove-min (c:remove-min th))) 0)
    (tst (c:remove-min (c:insert 0 0 empty-heap)) empty-heap)
    (tst (c:find-min-obj (c:insert 0 0 (c:remove-min (c:insert 0 0 empty-heap)))) 0)))

(let ([bad1 (make-kons (make-node 1 2 2 (make-kons (make-node 0 3 3 #f) #f)) (make-kons (make-node 0 1 1 #f) #f))]
      [bad2 (make-kons (make-node 0 1 1 #f) (make-kons (make-node 0 2 2 (make-kons (make-node 1 -1 -1 #f) #f)) #f))]
      [bad3 (make-kons (make-node 0 1 1 #f) (make-kons (make-node 1 2 2 (make-kons (make-node 0 -1 -1 #f) #f)) #f))])
  (begin
    (tst/x (c:find-min-obj bad1))
    (tst/x (node-children (kons-hd (node-children (kons-hd (c:insert 3 3 bad2))))))
    (tst/x (node-val (kons-hd (node-children (kons-hd (node-children (kons-hd (c:insert 3 3 bad3))))))))))

(let ([th (foldl (λ (x y) (o:insert x x y)) empty-heap (list 9 -123 59 85 52 2 0 -3 19283401924 59235))])
  (begin
    (tst (o:find-min-priority th) -123)
    (tst (o:find-min-obj th) -123)
    (tst (o:find-min-priority (o:remove-min th)) -3)
    (tst (o:find-min-obj (o:remove-min th)) -3)
    (tst (o:find-min-priority (o:remove-min (o:remove-min th))) 0)
    (tst (o:find-min-obj (o:remove-min (o:remove-min th))) 0)
    (tst (o:remove-min (o:insert 0 0 empty-heap)) empty-heap)
    (tst (o:find-min-obj (o:insert 0 0 (o:remove-min (o:insert 0 0 empty-heap)))) 0)))

(let ([bad1 (make-kons (make-node 1 2 2 (make-kons (make-node 0 3 3 #f) #f)) (make-kons (make-node 0 1 1 #f) #f))]
      [bad2 (make-kons (make-node 0 1 1 #f) (make-kons (make-node 0 2 2 (make-kons (make-node 1 -1 -1 #f) #f)) #f))]
      [bad3 (make-kons (make-node 0 1 1 #f) (make-kons (make-node 1 2 2 (make-kons (make-node 0 -1 -1 #f) #f)) #f))])
  (begin
    (tst/x (o:find-min-obj bad1))
    (tst/x (node-children (kons-hd (node-children (kons-hd (o:insert 3 3 bad2))))))
    (tst/x (node-val (kons-hd (node-children (kons-hd (node-children (kons-hd (o:insert 3 3 bad3))))))))))

(let ([th (foldl (λ (x y) (?:insert x x y)) empty-heap (list 9 -123 59 85 52 2 0 -3 19283401924 59235))])
  (begin
    (tst (?:find-min-priority th) -123)
    (tst (?:find-min-obj th) -123)
    (tst (?:find-min-priority (?:remove-min th)) -3)
    (tst (?:find-min-obj (?:remove-min th)) -3)
    (tst (?:find-min-priority (?:remove-min (?:remove-min th))) 0)
    (tst (?:find-min-obj (?:remove-min (?:remove-min th))) 0)
    (tst (?:remove-min (?:insert 0 0 empty-heap)) empty-heap)
    (tst (?:find-min-obj (?:insert 0 0 (?:remove-min (?:insert 0 0 empty-heap)))) 0)))

(let ([bad1 (make-kons (make-node 1 2 2 (make-kons (make-node 0 3 3 #f) #f)) (make-kons (make-node 0 1 1 #f) #f))]
      [bad2 (make-kons (make-node 0 1 1 #f) (make-kons (make-node 0 2 2 (make-kons (make-node 1 -1 -1 #f) #f)) #f))]
      [bad3 (make-kons (make-node 0 1 1 #f) (make-kons (make-node 1 2 2 (make-kons (make-node 0 -1 -1 #f) #f)) #f))])
  (begin
    (tst/x (?:find-min-obj bad1))
    (tst/x (node-children (kons-hd (node-children (kons-hd (?:insert 3 3 bad2))))))
    (tst/x (node-val (kons-hd (node-children (kons-hd (node-children (kons-hd (?:insert 3 3 bad3))))))))))

(let ([th (foldl (λ (x y) (h:insert x x y)) empty-heap (list 9 -123 59 85 52 2 0 -3 19283401924 59235))])
  (begin
    (tst (h:find-min-priority th) -123)
    (tst (h:find-min-obj th) -123)
    (tst (h:find-min-priority (h:remove-min th)) -3)
    (tst (h:find-min-obj (h:remove-min th)) -3)
    (tst (h:find-min-priority (h:remove-min (h:remove-min th))) 0)
    (tst (h:find-min-obj (h:remove-min (h:remove-min th))) 0)
    (tst (h:remove-min (h:insert 0 0 empty-heap)) empty-heap)
    (tst (h:find-min-obj (h:insert 0 0 (h:remove-min (h:insert 0 0 empty-heap)))) 0)))

(let ([bad1 (make-h:kons (make-h:node 1 2 2 (make-h:kons (make-h:node 0 3 3 #f) #f)) (make-h:kons (make-h:node 0 1 1 #f) #f))]
      [bad2 (make-h:kons (make-h:node 0 1 1 #f) (make-h:kons (make-h:node 0 2 2 (make-h:kons (make-h:node 1 -1 -1 #f) #f)) #f))]
      [bad3 (make-h:kons (make-h:node 0 1 1 #f) (make-h:kons (make-h:node 1 2 2 (make-h:kons (make-h:node 0 -1 -1 #f) #f)) #f))])
  (begin
    (tst/x (h:find-min-obj bad1))
    (tst/x (h:node-children (h:kons-hd (h:node-children (h:kons-hd (h:insert 3 3 bad2))))))
    (tst/x (h:node-val (h:kons-hd (h:node-children (h:kons-hd (h:node-children (h:kons-hd (h:insert 3 3 bad3))))))))))

(let ([th (foldl (λ (x y) (s:insert x x y)) empty-heap (list 9 -123 59 85 52 2 0 -3 19283401924 59235))])
  (begin
    (tst (s:find-min-priority th) -123)
    (tst (s:find-min-obj th) -123)
    (tst (s:find-min-priority (s:remove-min th)) -3)
    (tst (s:find-min-obj (s:remove-min th)) -3)
    (tst (s:find-min-priority (s:remove-min (s:remove-min th))) 0)
    (tst (s:find-min-obj (s:remove-min (s:remove-min th))) 0)
    (tst (s:remove-min (s:insert 0 0 empty-heap)) empty-heap)
    (tst (s:find-min-obj (s:insert 0 0 (s:remove-min (s:insert 0 0 empty-heap)))) 0)))

(let ([bad1 (make-h:kons (make-h:node 1 2 2 (make-h:kons (make-h:node 0 3 3 #f) #f)) (make-h:kons (make-h:node 0 1 1 #f) #f))]
      [bad2 (make-h:kons (make-h:node 0 1 1 #f) (make-h:kons (make-h:node 0 2 2 (make-h:kons (make-h:node 1 -1 -1 #f) #f)) #f))]
      [bad3 (make-h:kons (make-h:node 0 1 1 #f) (make-h:kons (make-h:node 1 2 2 (make-h:kons (make-h:node 0 -1 -1 #f) #f)) #f))])
  (begin
    (tst/x (s:find-min-obj bad1))
    (tst/x (format "~s" (s:insert 3 3 bad2))) ;; printing it out traverses it, triggering the contract check
    (tst/x (h:node-val (h:kons-hd (h:node-children (h:kons-hd (h:node-children (h:kons-hd (s:insert 3 3 bad3))))))))))

(let ([th (foldl (λ (x y) (t:insert x x y)) empty-heap (list 9 -123 59 85 52 2 0 -3 19283401924 59235))])
  (begin
    (tst (t:find-min-priority th) -123)
    (tst (t:find-min-obj th) -123)
    (tst (t:find-min-priority (t:remove-min th)) -3)
    (tst (t:find-min-obj (t:remove-min th)) -3)
    (tst (t:find-min-priority (t:remove-min (t:remove-min th))) 0)
    (tst (t:find-min-obj (t:remove-min (t:remove-min th))) 0)
    (tst (t:remove-min (t:insert 0 0 empty-heap)) empty-heap)
    (tst (t:find-min-obj (t:insert 0 0 (t:remove-min (t:insert 0 0 empty-heap)))) 0)))

(let ([bad1 (make-h:kons (make-h:node 1 2 2 (make-h:kons (make-h:node 0 3 3 #f) #f)) (make-h:kons (make-h:node 0 1 1 #f) #f))]
      [bad2 (make-h:kons (make-h:node 0 1 1 #f) (make-h:kons (make-h:node 0 2 2 (make-h:kons (make-h:node 1 -1 -1 #f) #f)) #f))]
      [bad3 (make-h:kons (make-h:node 0 1 1 #f) (make-h:kons (make-h:node 1 2 2 (make-h:kons (make-h:node 0 -1 -1 #f) #f)) #f))])
  (begin
    (tst/x (t:find-min-obj bad1))
    (tst/x (format "~s" (t:insert 3 3 bad2))) ;; printing it out traverses it, triggering the contract check
    (tst/x (h:node-val (h:kons-hd (h:node-children (h:kons-hd (h:node-children (h:kons-hd (t:insert 3 3 bad3))))))))))

(if (= 0 failing-tests)
    (printf "~a tests, all passed.\n" tests)
    (printf "~a tests out of ~a total failed.\n" failing-tests tests))
