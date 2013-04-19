#lang racket
;;; Science Collection
;;; fft.rkt
;;; Copyright (c) 2011 M. Douglas Williams
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
;;; Version  Date      Description
;;; 4.0.0    08/18/11  Changed the header and restructured the code. (MDW)
;;; 

(require "math.rkt"
         racket/unsafe/ops
         "unsafe-ops-utils.rkt")

;;; fft-forward : real? = -1.0
;;; fft-backward : real? = +1.0
(define fft-forward -1.0)
(define fft-backward +1.0)

;;; Radix 2 Complex FFT

;;; (fft-bitreverse-order data) -> void?
;;;   data : (vectorof complex?)
;;; Implements the Gold-Rader bit-reversal algorithm. Note that this is an
;;; internal routine and all operations are unsafe.
(define (fft-bitreverse-order data)
  (let ((n (unsafe-vector-length data)))
    (for/fold ((j 0))
              ((i (in-range (unsafe-fx- n 1))))
      (let ((k (unsafe-fxquotient n 2)))
        (when (unsafe-fx< i j)
          (let ((tmp (unsafe-vector-ref data i)))
            (unsafe-vector-set! data i (unsafe-vector-ref data j))
            (unsafe-vector-set! data j tmp)))
        (let loop ()
          (when (unsafe-fx<= k j)
            (set! j (unsafe-fx- j k))
            (set! k (unsafe-fxquotient k 2))
            (loop)))
        (unsafe-fx+ j k)))
    (void)))

;;; (fft-binary-logn n) -> (or/c exact-nonnegative-integer? false/c)
;;;   n : exact-nonegative-integer?
;;; If n is a power of 2, returns the base 2 log of n (i.e.,
;;; (expt (fft-binary-logn n) 2) = n), otherwise #f is returned. Note that this
;;; is an internal routine and all operations are unsafe.
(define (fft-binary-logn n)
  (with-fixed (n)
    (let ((binary-logn
           (let loop ((k 1)
                      (l 0))
             (if (unsafe-fx>= k n)
                 l
                 (loop (unsafe-fx* k 2) (unsafe-fx+ l 1))))))
      (if (unsafe-fx= n (unsafe-fxlshift 1 binary-logn))
          binary-logn
          #f))))

;;; Decimation in Time

;;; (fft-complex-radix2-forward data) -> void?
;;;   data (vectorof complex?)
(define (fft-complex-radix2-forward data)
  (fft-complex-radix2-transform data fft-forward))

;;; (fft-complex-radix2-backward data) -> void?
;;;   data (vectorof complex?)
(define (fft-complex-radix2-backward data)
  (fft-complex-radix2-transform data fft-backward))

;;; (fft-complex-radix2-inverse data) -> void?
;;;   data (vectorof complex?)
(define (fft-complex-radix2-inverse data)
  (fft-complex-radix2-backward data)
  ;; Normalize inverse FFT with 1/n.
  (let* ((n (unsafe-vector-length data))
         (norm (unsafe-fl/ 1.0 (unsafe-fx->fl n))))
    (for ((i (in-range n)))
      (let* ((datum (unsafe-vector-ref data i))
             (datum-real (real-part datum))
             (datum-imag (imag-part datum)))
        (unsafe-vector-set!
         data i
         (make-rectangular
          (unsafe-fl* norm datum-real)
          (unsafe-fl* norm datum-imag)))))))

;;; (fft-complex-radix2-transform data sign) -> void?
;;;   data : (vectorof real?)
;;;   sign : (one-of/c -1.0 1.0)
(define (fft-complex-radix2-transform data sign)
  (with-float (sign)
    (let* ((n (vector-length data))
           (logn (fft-binary-logn n)))
      ;; Ensure vector length is a power of 2.
      (unless logn
        (error 'fft-complex-radix2-transform
               "vector length (~a) is not a power of 2"
               n))
      ;; We don't need to do anything if n = 1.
      (unless (unsafe-fx= n 1)
        ;; Bit reverse the ordering of input data for decimation in time
        ;; algorithm.
        (fft-bitreverse-order data)
        ;; Apply FFT recursion.
        (for/fold ((dual 1))
                  ((bit (in-range logn)))
          (let* ((w-real 1.0)
                 (w-imag 0.0)
                 (theta (unsafe-fl/
                         (unsafe-fl* sign 2*pi)
                         (unsafe-fl* 2.0 (unsafe-fx->fl dual))))
                 (s (unsafe-flsin theta))
                 (t (unsafe-flsin (unsafe-fl/ theta 2.0)))
                 (s2 (unsafe-fl* 2.0 (unsafe-fl* t t))))
            ;; a = 0
            (for ((b (in-range 0 n (unsafe-fx* 2 dual))))
              (let* ((i b)
                     (j (unsafe-fx+ b dual))
                     (wd (unsafe-vector-ref data j)))
                (unsafe-vector-set!
                 data j (- (unsafe-vector-ref data i) wd))
                (unsafe-vector-set!
                 data i (+ (unsafe-vector-ref data i) wd))))
            ;; a = 1 .. (dual - 1)
            (for ((a (in-range 1 dual)))
              ;; Trignometric recurrence for w -> exp(i theta) w
              (let ((tmp-real
                     (unsafe-fl-
                      (unsafe-fl- w-real (unsafe-fl* s w-imag))
                      (unsafe-fl* s2 w-real)))
                    (tmp-imag
                     (unsafe-fl-
                      (unsafe-fl+ w-imag (unsafe-fl* s w-real))
                      (unsafe-fl* s2 w-imag))))
                (set! w-real tmp-real)
                (set! w-imag tmp-imag))
              (for ((b (in-range 0 n (unsafe-fx* 2 dual))))
                (let* ((i (unsafe-fx+ b a))
                       (j (unsafe-fx+ (unsafe-fx+ b a) dual))
                       (z1 (unsafe-vector-ref data j))
                       (z1-real (real-part z1))
                       (z1-imag (imag-part z1)))
                  (with-float (z1-real z1-imag)
                    (let ((wd (make-rectangular
                               (unsafe-fl-
                                (unsafe-fl* w-real z1-real)
                                (unsafe-fl* w-imag z1-imag))
                               (unsafe-fl+
                                (unsafe-fl* w-real z1-imag)
                                (unsafe-fl* w-imag z1-real)))))
                      (unsafe-vector-set!
                       data j (- (unsafe-vector-ref data i) wd))
                      (unsafe-vector-set!
                       data i (+ (unsafe-vector-ref data i) wd)))))))
            (unsafe-fx* dual 2))))
      ;; Return void per the contract.
      (void))))

;;; Decimation in Frequency

;;; (fft-complex-radix2-dif-forward data) -> void?
;;;   data (vectorof complex?)
(define (fft-complex-radix2-dif-forward data)
  (fft-complex-radix2-dif-transform data fft-forward))

;;; (fft-complex-radix2-dif-backward data) -> void?
;;;   data (vectorof complex?)
(define (fft-complex-radix2-dif-backward data)
  (fft-complex-radix2-dif-transform data fft-backward))

;;; (fft-complex-radix2-dif-inverse data) -> void?
;;;   data (vectorof complex?)
(define (fft-complex-radix2-dif-inverse data)
  (fft-complex-radix2-dif-backward data)
  ;; Normalize inverse FFT with 1/n.
  (let* ((n (unsafe-vector-length data))
         (norm (unsafe-fl/ 1.0 (unsafe-fx->fl n))))
    (for ((i (in-range n)))
      (let* ((datum (unsafe-vector-ref data i))
             (datum-real (real-part datum))
             (datum-imag (imag-part datum)))
        (unsafe-vector-set!
         data i
         (make-rectangular
          (unsafe-fl* norm datum-real)
          (unsafe-fl* norm datum-imag)))))))

;;; (fft-complex-radix2-dif-transform data sign) -> void?
;;;   data : (vectorof real?)
;;;   sign : (one-of/c -1.0 1.0)
(define (fft-complex-radix2-dif-transform data sign)
  (with-float (sign)
   (let* ((n (vector-length data))
          (logn (fft-binary-logn n)))
     ;; Ensure vector length is a power of 2.
     (unless logn
       (error 'fft-complex-radix2-transform
              "vector length (~a) is not a power of 2"
              n))
     ;; We don't need to do anything if n = 1.
     (unless (unsafe-fx= n 1)
       ;; Apply FFT recursion.
       (for/fold ((dual (unsafe-fxquotient n 2)))
                 ((bit (in-range logn)))
         (let* ((w-real 1.0)
                (w-imag 0.0)
                (theta (unsafe-fl/
                        (unsafe-fl* sign 2*pi)
                        (unsafe-fl* 2.0 (unsafe-fx->fl dual))))
                (s (unsafe-flsin theta))
                (t (unsafe-flsin (unsafe-fl/ theta 2.0)))
                (s2 (unsafe-fl* 2.0 (unsafe-fl* t t))))
           (for ((b (in-range dual)))
             (for ((a (in-range 0 n (unsafe-fx* 2 dual))))
               (let* ((i (unsafe-fx+ b a))
                      (j (unsafe-fx+ (unsafe-fx+ b a) dual))
                      (t1 (+ (unsafe-vector-ref data i)
                             (unsafe-vector-ref data j)))
                      (t2 (- (unsafe-vector-ref data i)
                             (unsafe-vector-ref data j)))
                      (t2-real (real-part t2))
                      (t2-imag (imag-part t2)))
                 (with-float (t2-real t2-imag)
                   (unsafe-vector-set!
                    data i t1)
                   (unsafe-vector-set!
                    data j
                    (make-rectangular
                     (unsafe-fl-
                      (unsafe-fl* w-real t2-real)
                      (unsafe-fl* w-imag t2-imag))
                     (unsafe-fl+
                      (unsafe-fl* w-real t2-imag)
                      (unsafe-fl* w-imag t2-real)))))))
             (let ((tmp-real
                    (unsafe-fl-
                     (unsafe-fl- w-real (unsafe-fl* s w-imag))
                     (unsafe-fl* s2 w-real)))
                   (tmp-imag
                    (unsafe-fl-
                     (unsafe-fl+ w-imag (unsafe-fl* s w-real))
                     (unsafe-fl* s2 w-imag))))
               (set! w-real tmp-real)
               (set! w-imag tmp-imag)))
           (unsafe-fxquotient dual 2)))
       ;; Bit reverse the order of the output data for decimation in frequency
       ;; algorithm.
       (fft-bitreverse-order data))
     ;; Return void per the contract.
     (void))))

;;; Multi-Radix Complex FFT

;;; (fft-complex-factorize n)
;;; -> (listof fixnum?)
;;;   n : fixnum?
(define (fft-complex-factorize n)
  (fft-factorize n '(7 6 5 4 3 2)))

;;; (fft-factorize n implemented-subtransforms) -> (listof fixnum?)
;;;   n : fixnum?
;;;   implemented-subtransforms : (listof fixnum?)
(define (fft-factorize n implemented-subtransforms)
  (with-fixed (n)
    (if (unsafe-fx= n 1)
        '(1)
        (let ((ntest n)
              (factors '()))
          ;; Deal with implemented factors first.
          (let outer-loop ()
            (unless (or (null? implemented-subtransforms)
                        (unsafe-fx= ntest 0))
              (let ((factor (car implemented-subtransforms)))
                (let inner-loop ()
                  (when (unsafe-fx= (unsafe-fxmodulo ntest factor) 0)
                    (set! factors (append factors (list factor)))
                    (set! ntest (unsafe-fxquotient ntest factor))
                    (inner-loop)))
                (set! implemented-subtransforms
                      (cdr implemented-subtransforms)))
              (outer-loop)))
          ;; Deal with any other even prime factors (there is only one).
          (let ((factor 2))
            (let loop ()
              (when (and (not (unsafe-fx= ntest 1))
                         (unsafe-fx= (unsafe-fxmodulo ntest factor) 0))
                (set! factors (append factors (list factor)))
                (set! ntest (unsafe-fxquotient ntest factor))
                (loop))))
          ;; Deal with any other odd prime factors.
          (let ((factor 3))
            (let outer-loop ()
              (unless (unsafe-fx= ntest 1)
                (let inner-loop ()
                  (unless (unsafe-fx= (unsafe-fxmodulo ntest factor) 0)
                    (set! factor (unsafe-fx+ factor 2))
                    (inner-loop)))
                (set! factors (append factors (list factor)))
                (set! ntest (unsafe-fxquotient ntest factor))
                (outer-loop))))
          factors))))

;;; (struct fft-complex-wavetable (n
;;;                                nf
;;;                                factors
;;;                                twiddle
;;;                                trig))
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   nf : (and/c exact-positive-integer? fixnum?)
;;;   factors : (listof (and/c exact-positive-integer? fixnum?))
;;;   twiddle : (vectorof fixnum?)
;;;   trig : (vectorof complex?)
(struct fft-complex-wavetable
  (n
   nf
   factors
   twiddle
   trig))

;;; fft-complex-wavetable-hash : hash-eq?
(define fft-complex-wavetable-hash (make-hasheq))

;;; (get-fft-complex-wavetable n) -> fft-complex-wavetable?
;;;   n : (and/c exact-positive-integer? fixnum?)
(define (get-fft-complex-wavetable n)
  (with-fixed (n)
    (hash-ref! fft-complex-wavetable-hash n
               (make-fft-complex-wavetable n))))

;;; (make-fft-complex-wavetable n) -> fft-complex-wavetable?
;;;   n : (and/c exact-positive-integer? fixnum?)
;;; Creates a new wavetable for vectors of length n.
(define (make-fft-complex-wavetable n)
  (let* ((factors (fft-complex-factorize n))
         (nf (length factors))
         (twiddle (make-vector nf))
         (trig (make-vector n)))
    (let ((d-theta (unsafe-fl/ (unsafe-fl- 0.0 2*pi)
                               (unsafe-fx->fl n)))
          (t 0)
          (product 1)
          (product-1 0)
          (q 0))
      (for ((factor (in-list factors))
            (i (in-naturals)))
        (unsafe-vector-set! twiddle i t)
        (set! product-1 product) ; product-1 = p_(i-1)
        (set! product (unsafe-fx* product factor))
        (set! q (unsafe-fxquotient n product))
        (for ((j (in-range 1 factor)))
          (let ((m 0))
            (for ((k (in-range 1 (unsafe-fx+ q 1))))
              (set! m (unsafe-fx+ m (unsafe-fx* j product-1)))
              (set! m (unsafe-fxmodulo m n))
              (let ((theta
                     (unsafe-fl* d-theta
                                 (unsafe-fx->fl m)))) ; d-theta*j*k*p_(i-1)
                (unsafe-vector-set!
                 trig t (make-rectangular
                         (unsafe-flcos theta) (unsafe-flsin theta))))
              (set! t (unsafe-fx+ t 1)))))))
    (fft-complex-wavetable
     n nf factors twiddle trig)))

;;; (struct fft-complex-workspace (n
;;;                                scratch))
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   scratch : (vectorof complex?)
(struct fft-complex-workspace
  (n
   scratch))

;;; (make-fft-complex-workspace n)
;;;   n : (and/c exact-positive-integer? fixnum?)
(define (make-fft-complex-workspace n)
  (fft-complex-workspace n (make-vector n)))

;;; (fft-complex-forward data #:workspace workspace) -> void?
;;;   data : (vectorof complex?)
;;;   workspace : fft-complex-workspace
;;;     = (make-fft-complex-workspace (vector-length data))
(define (fft-complex-forward
         data
         #:workspace
         (workspace (make-fft-complex-workspace (vector-length data))))
  (fft-complex-transform data fft-forward #:workspace workspace))

;;; (fft-complex-backward data #:workspace workspace) -> void?
;;;   data : (vectorof complex?)
;;;   workspace : fft-complex-workspace
;;;     = (make-fft-complex-workspace (vector-length data))
(define (fft-complex-backward
         data
         #:workspace
         (workspace (make-fft-complex-workspace (vector-length data))))
  (fft-complex-transform data fft-backward #:workspace workspace))

;;; (fft-complex-inverse data #:workspace workspace) -> void?
;;;   data : (vectorof complex?)
;;;   workspace : fft-complex-workspace
;;;     = (make-fft-complex-workspace (vector-length data))
(define (fft-complex-inverse
         data
         #:workspace
         (workspace (make-fft-complex-workspace (vector-length data))))
  (fft-complex-transform data fft-backward #:workspace workspace)
  ;; Normalize inverse FFT with 1/n.
  (let* ((n (unsafe-vector-length data))
         (norm (unsafe-fl/ 1.0 (unsafe-fx->fl n))))
    (for ((i (in-range n)))
      (let* ((datum (unsafe-vector-ref data i))
             (datum-real (real-part datum))
             (datum-imag (imag-part datum)))
        (unsafe-vector-set!
         data i
         (make-rectangular
          (unsafe-fl* norm datum-real)
          (unsafe-fl* norm datum-imag)))))))

;;; (fft-complex-transform data sign #:workspace workspace) -> void?
;;;   data : (vectorof complex?)
;;;   sign : (or/c -1.0 1.0)
;;;   workspace : fft-complex-workspace
;;;     = (make-fft-complex-workspace (vector-length data))
(define (fft-complex-transform
         data sign
         #:workspace
         (workspace (make-fft-complex-workspace (vector-length data))))
  (let ((n (vector-length data)))
    (unless (unsafe-fx= n (fft-complex-workspace-n workspace))
      (error 'fft-complex-transform
             "workspace does not match length of data, ~a"
             n))
    (let* ((wavetable (get-fft-complex-wavetable n))
           (nf (fft-complex-wavetable-nf wavetable))
           (factors (fft-complex-wavetable-factors wavetable))
           (twiddle (fft-complex-wavetable-twiddle wavetable))
           (trig (fft-complex-wavetable-trig wavetable))
           (scratch (fft-complex-workspace-scratch workspace))
           (state 0)
           (in data)
           (out scratch)
           (product 1)
           (q 0))
      (unless (unsafe-fx= n 1)
        (for ((factor (in-list factors))
              (i (in-naturals)))
          (set! product (unsafe-fx* product factor))
          (set! q (unsafe-fxquotient n product))
          (if (unsafe-fx= state 0)
              (begin
                (set! in data)
                (set! out scratch)
                (set! state 1))
              (begin
                (set! in scratch)
                (set! out data)
                (set! state 0)))
          (cond ((unsafe-fx= factor 2)
                 (let ((twiddle1 (unsafe-vector-ref twiddle i)))
                   (fft-complex-pass-2
                    in out sign product n
                    trig twiddle1)))
                ((unsafe-fx= factor 3)
                 (let* ((twiddle1 (unsafe-vector-ref twiddle i))
                        (twiddle2 (unsafe-fx+ twiddle1 q)))
                   (fft-complex-pass-3
                    in out sign product n
                    trig twiddle1 twiddle2)))
                ((unsafe-fx= factor 4)
                 (let* ((twiddle1 (unsafe-vector-ref twiddle i))
                        (twiddle2 (unsafe-fx+ twiddle1 q))
                        (twiddle3 (unsafe-fx+ twiddle2 q)))
                   (fft-complex-pass-4
                    in out sign product n
                    trig twiddle1 twiddle2 twiddle3)))
                ((unsafe-fx= factor 5)
                 (let* ((twiddle1 (unsafe-vector-ref twiddle i))
                        (twiddle2 (unsafe-fx+ twiddle1 q))
                        (twiddle3 (unsafe-fx+ twiddle2 q))
                        (twiddle4 (unsafe-fx+ twiddle3 q)))
                   (fft-complex-pass-5
                    in out sign product n trig
                    twiddle1 twiddle2 twiddle3 twiddle4)))
                ((unsafe-fx= factor 6)
                 (let* ((twiddle1 (unsafe-vector-ref twiddle i))
                        (twiddle2 (unsafe-fx+ twiddle1 q))
                        (twiddle3 (unsafe-fx+ twiddle2 q))
                        (twiddle4 (unsafe-fx+ twiddle3 q))
                        (twiddle5 (unsafe-fx+ twiddle4 q)))
                   (fft-complex-pass-6
                    in out sign product n trig
                    twiddle1 twiddle2 twiddle3 twiddle4 twiddle5)))
                ((unsafe-fx= factor 7)
                 (let* ((twiddle1 (unsafe-vector-ref twiddle i))
                        (twiddle2 (unsafe-fx+ twiddle1 q))
                        (twiddle3 (unsafe-fx+ twiddle2 q))
                        (twiddle4 (unsafe-fx+ twiddle3 q))
                        (twiddle5 (unsafe-fx+ twiddle4 q))
                        (twiddle6 (unsafe-fx+ twiddle5 q)))
                   (fft-complex-pass-7
                    in out sign product n trig
                    twiddle1 twiddle2 twiddle3 twiddle4 twiddle5 twiddle6)))
                (else
                 (let ((twiddle1 (unsafe-vector-ref twiddle i)))
                   (fft-complex-pass-n
                    in out sign factor product n
                    trig twiddle1)))))
        (when (unsafe-fx= state 1)
          (for ((i (in-range n)))
            (unsafe-vector-set! data i (unsafe-vector-ref scratch i))))))))

;;; (fft-complex-pass-2 in out sign product n trig
;;;                     twiddle) -> void
;;;   in : (vectorof complex?)
;;;   out : (vectorof complex?)
;;;   sign : (one-of/c -1.0 1.0)
;;;   product : (and/c exact-positive-integer? fixnum?)
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   trig : (vectorof complex?)
;;;   twiddle : (and/c exact-nonnegative-integer? fixnum?)
(define (fft-complex-pass-2 in out sign product n
                            trig twiddle)
  (let* ((i 0)
         (j 0)
         (factor 2)
         (m (unsafe-fxquotient n factor))
         (q (unsafe-fxquotient n product))
         (product-1 (unsafe-fxquotient product factor))
         (jump (unsafe-fx* (unsafe-fx- factor 1) product-1)))
    (for ((k (in-range q)))
      (let ((w-real 1.0)
            (w-imag 0.0))
        (unless (= k 0)
          (if (= sign fft-forward)
              (begin
                ;; Forward transform
                (set! w-real (real-part (vector-ref trig (+ twiddle (- k 1)))))
                (set! w-imag (imag-part (vector-ref trig (+ twiddle (- k 1))))))
              (begin
                ;; Backward transform: w -> conjugate(w)
                (set! w-real (real-part (vector-ref trig (+ twiddle (- k 1)))))
                (set! w-imag (- (imag-part (vector-ref trig (+ twiddle (- k 1)))))))))
        (for ((k1 (in-range product-1)))
          (let* ((z0 (unsafe-vector-ref in i))
                 (z0-real (real-part z0))
                 (z0-imag (imag-part z0))
                 (z1 (unsafe-vector-ref in (unsafe-fx+ i m)))
                 (z1-real (real-part z1))
                 (z1-imag (imag-part z1)))
            (with-float (z0-real z0-imag z1-real z1-imag)
              ;; Compute x = W(2) z
              (let (;; x0 = z0 + z1
                    (x0-real (unsafe-fl+ z0-real z1-real))
                    (x0-imag (unsafe-fl+ z0-imag z1-imag))
                    ;; x1 = z0 - z1
                    (x1-real (unsafe-fl- z0-real z1-real))
                    (x1-imag (unsafe-fl- z0-imag z1-imag)))
                ;; Apply twiddle factors
                ;; out0 = 1 * x0
                (unsafe-vector-set!
                 out j (make-rectangular x0-real x0-imag))
                ;; out1 = w * x1
                (unsafe-vector-set!
                 out (+ j product-1)
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w-real x1-real) (unsafe-fl* w-imag x1-imag))
                  (unsafe-fl+ (unsafe-fl* w-real x1-imag) (unsafe-fl* w-imag x1-real))))))
            (set! i (unsafe-fx+ i 1))
            (set! j (unsafe-fx+ j 1))))
        (set! j (unsafe-fx+ j jump))))))

;;; (fft-complex-pass-3 in out sign product n trig
;;;                     twiddle1 twiddle2) -> void
;;;   in : (vectorof complex?)
;;;   out : (vectorof complex?)
;;;   sign : (one-of/c -1.0 1.0)
;;;   product : (and/c exact-positive-integer? fixnum?)
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   trig : (vectorof complex?)
;;;   twiddle1 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle2 : (and/c exact-nonnegative-integer? fixnum?)
(define (fft-complex-pass-3 in out sign product n
                            trig twiddle1 twiddle2)
  (define tau (unsafe-fl/ (unsafe-flsqrt 3.0) 2.0))
  (let* ((i 0)
         (j 0)
         (factor 3)
         (m (unsafe-fxquotient n factor))
         (q (unsafe-fxquotient n product))
         (p-1 (unsafe-fxquotient product factor))
         (jump (unsafe-fx* (unsafe-fx- factor 1) p-1)))
    (for ((k (in-range q)))
      (let ((w1-real 1.0)
            (w1-imag 0.0)
            (w2-real 1.0)
            (w2-imag 0.0))
        (unless (unsafe-fx= k 0)
          (let ((tw1 (unsafe-vector-ref trig (unsafe-fx+ twiddle1 (unsafe-fx- k 1))))
                (tw2 (unsafe-vector-ref trig (unsafe-fx+ twiddle2 (unsafe-fx- k 1)))))
            (if (= sign fft-forward)
                (begin
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (imag-part tw1))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (imag-part tw2)))
                (begin
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (- (imag-part tw1)))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (- (imag-part tw2)))))))
        (for ((k1 (in-range p-1)))
          (let* ((z0 (unsafe-vector-ref in i))
                 (z0-real (real-part z0))
                 (z0-imag (imag-part z0))
                 (z1 (unsafe-vector-ref in (unsafe-fx+ i m)))
                 (z1-real (real-part z1))
                 (z1-imag (imag-part z1))
                 (z2 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 2 m))))
                 (z2-real (real-part z2))
                 (z2-imag (imag-part z2)))
            (with-float (z0-real z0-imag z1-real z1-imag z2-real z2-imag)
              ;; Compute x = W(3) z
              (let* (;; t1 = z1 + z2
                     (t1-real (unsafe-fl+ z1-real z2-real))
                     (t1-imag (unsafe-fl+ z1-imag z2-imag))
                     ;; t2 = z0 - t1/2
                     (t2-real (unsafe-fl- z0-real (unsafe-fl/ t1-real 2.0)))
                     (t2-imag (unsafe-fl- z0-imag (unsafe-fl/ t1-imag 2.0)))
                     ;; t3 = (+/1) sin (pi/3) * (z1 - z2)
                     (t3-real (* sign tau (unsafe-fl- z1-real z2-real)))
                     (t3-imag (* sign tau (unsafe-fl- z1-imag z2-imag)))
                     ;; x0 = z0 + t1
                     (x0-real (unsafe-fl+ z0-real t1-real))
                     (x0-imag (unsafe-fl+ z0-imag t1-imag))
                     ;; x1 = t2 + i t3
                     (x1-real (unsafe-fl- t2-real t3-imag))
                     (x1-imag (unsafe-fl+ t2-imag t3-real))
                     ;; x2 = t2 - i t3
                     (x2-real (unsafe-fl+ t2-real t3-imag))
                     (x2-imag (unsafe-fl- t2-imag t3-real)))
                ;; Apply twiddle factors
                ;; to0 = 1 * x0
                (vector-set!
                 out j (make-rectangular x0-real x0-imag))
                ;; to1 = w1 * x1
                (vector-set!
                 out (unsafe-fx+ j p-1)
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w1-real x1-real)
                              (unsafe-fl* w1-imag x1-imag))
                  (unsafe-fl+ (unsafe-fl* w1-real x1-imag)
                              (unsafe-fl* w1-imag x1-real))))
                ;; to2 = w2 * x2
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 2 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w2-real x2-real)
                              (unsafe-fl* w2-imag x2-imag))
                  (unsafe-fl+ (unsafe-fl* w2-real x2-imag)
                              (unsafe-fl* w2-imag x2-real))))))
            (set! i (unsafe-fx+ i 1))
            (set! j (unsafe-fx+ j 1))))
        (set! j (unsafe-fx+ j jump))))))

;;; (fft-complex-pass-4 in out sign product n trig
;;;                     twiddle1 twiddle2 twiddle3) -> void
;;;   in : (vectorof complex?)
;;;   out : (vectorof complex?)
;;;   sign : (one-of/c -1.0 1.0)
;;;   product : (and/c exact-positive-integer? fixnum?)
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   trig : (vectorof complex?)
;;;   twiddle1 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle2 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle3 : (and/c exact-nonnegative-integer? fixnum?)
(define (fft-complex-pass-4 in out sign product n
                            trig twiddle1 twiddle2 twiddle3)
  (let* ((i 0)
         (j 0)
         (factor 4)
         (m (unsafe-fxquotient n factor))
         (q (unsafe-fxquotient n product))
         (p-1 (unsafe-fxquotient product factor))
         (jump (unsafe-fx* (unsafe-fx- factor 1) p-1)))
    (for ((k (in-range q)))
      (let ((w1-real 1.0)
            (w1-imag 0.0)
            (w2-real 1.0)
            (w2-imag 0.0)
            (w3-real 1.0)
            (w3-imag 0.0))
        (unless (unsafe-fx= k 0)
          (let ((tw1 (unsafe-vector-ref trig (unsafe-fx+ twiddle1 (unsafe-fx- k 1))))
                (tw2 (unsafe-vector-ref trig (unsafe-fx+ twiddle2 (unsafe-fx- k 1))))
                (tw3 (unsafe-vector-ref trig (unsafe-fx+ twiddle3 (unsafe-fx- k 1)))))
            (if (= sign fft-forward)
                (begin
                  ;; Forward transform
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (imag-part tw1))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (imag-part tw2))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (imag-part tw3)))
                (begin
                  ;; Backward transform : w -> conjugate(w)
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (- (imag-part tw1)))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (- (imag-part tw2)))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (- (imag-part tw3)))))))
        (for ((k1 (in-range p-1)))
          (let* ((z0 (unsafe-vector-ref in i))
                 (z0-real (real-part z0))
                 (z0-imag (imag-part z0))
                 (z1 (unsafe-vector-ref in (unsafe-fx+ i m)))
                 (z1-real (real-part z1))
                 (z1-imag (imag-part z1))
                 (z2 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 2 m))))
                 (z2-real (real-part z2))
                 (z2-imag (imag-part z2))
                 (z3 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 3 m))))
                 (z3-real (real-part z3))
                 (z3-imag (imag-part z3)))
            (with-float (z0-real z0-imag z1-real z1-imag
                         z2-real z2-imag z3-real z3-imag)
              ;; Compute x = W(4) z          
              (let* (;; t1 = z0 + z2
                     (t1-real (unsafe-fl+ z0-real z2-real))
                     (t1-imag (unsafe-fl+ z0-imag z2-imag))
                     ;; t2 = z1 + z3
                     (t2-real (unsafe-fl+ z1-real z3-real))
                     (t2-imag (unsafe-fl+ z1-imag z3-imag))
                     ;; t3 = z0 - z2
                     (t3-real (unsafe-fl- z0-real z2-real))
                     (t3-imag (unsafe-fl- z0-imag z2-imag))
                     ;; t4 = (+/-) (z1 - z3)
                     (t4-real (* sign (unsafe-fl- z1-real z3-real)))
                     (t4-imag (* sign (unsafe-fl- z1-imag z3-imag)))
                     ;; x0 = t1 + t2
                     (x0-real (unsafe-fl+ t1-real t2-real))
                     (x0-imag (unsafe-fl+ t1-imag t2-imag))
                     ;; x1 = t3 + i t4
                     (x1-real (unsafe-fl- t3-real t4-imag))
                     (x1-imag (unsafe-fl+ t3-imag t4-real))
                     ;; x2 = t1 - t2
                     (x2-real (unsafe-fl- t1-real t2-real))
                     (x2-imag (unsafe-fl- t1-imag t2-imag))
                     ;; x3 = t3 - i t4
                     (x3-real (unsafe-fl+ t3-real t4-imag))
                     (x3-imag (unsafe-fl- t3-imag t4-real)))
                ;; Apply twiddle factors
                ;; to0 = 1 * x0
                (vector-set!
                 out j (make-rectangular x0-real x0-imag))
                ;; to1 = w1 * x1
                (vector-set!
                 out (unsafe-fx+ j p-1)
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w1-real x1-real)
                              (unsafe-fl* w1-imag x1-imag))
                  (unsafe-fl+ (unsafe-fl* w1-real x1-imag)
                              (unsafe-fl* w1-imag x1-real))))
                ;; to2 = w2 * x2
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 2 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w2-real x2-real)
                              (unsafe-fl* w2-imag x2-imag))
                  (unsafe-fl+ (unsafe-fl* w2-real x2-imag)
                              (unsafe-fl* w2-imag x2-real))))
                ;; to3 = w3 * x3
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 3 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w3-real x3-real)
                              (unsafe-fl* w3-imag x3-imag))
                  (unsafe-fl+ (unsafe-fl* w3-real x3-imag)
                              (unsafe-fl* w3-imag x3-real))))))
            (set! i (unsafe-fx+ i 1))
            (set! j (unsafe-fx+ j 1))))
        (set! j (unsafe-fx+ j jump))))))

;;; (fft-complex-pass-5 in out sign product n trig
;;;                     twiddle1 twiddle2 twiddle3
;;;                     twiddle4) -> void
;;;   in : (vectorof complex?)
;;;   out : (vectorof complex?)
;;;   sign : (one-of/c -1.0 1.0)
;;;   product : (and/c exact-positive-integer? fixnum?)
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   trig : (vectorof complex?)
;;;   twiddle1 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle2 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle3 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle4 : (and/c exact-nonnegative-integer? fixnum?)
(define (fft-complex-pass-5 in out sign product n trig
                            twiddle1 twiddle2 twiddle3 twiddle4)
  (define sqrt-5-by-4 (unsafe-fl/ (unsafe-flsqrt 5.0) 4.0))
  (define sin-2pi-by-5 (unsafe-flsin (unsafe-fl/ 2*pi 5.0)))
  (define sin-2pi-by-10 (unsafe-flsin (unsafe-fl/ 2*pi 10.0)))
  (let* ((i 0)
         (j 0)
         (factor 5)
         (m (unsafe-fxquotient n factor))
         (q (unsafe-fxquotient n product))
         (p-1 (unsafe-fxquotient product factor))
         (jump (unsafe-fx* (unsafe-fx- factor 1) p-1)))
    (for ((k (in-range q)))
      (let ((w1-real 1.0)
            (w1-imag 0.0)
            (w2-real 1.0)
            (w2-imag 0.0)
            (w3-real 1.0)
            (w3-imag 0.0)
            (w4-real 1.0)
            (w4-imag 0.0))
        (unless (unsafe-fx= k 0)
          (let ((tw1 (unsafe-vector-ref trig (unsafe-fx+ twiddle1 (unsafe-fx- k 1))))
                (tw2 (unsafe-vector-ref trig (unsafe-fx+ twiddle2 (unsafe-fx- k 1))))
                (tw3 (unsafe-vector-ref trig (unsafe-fx+ twiddle3 (unsafe-fx- k 1))))
                (tw4 (unsafe-vector-ref trig (unsafe-fx+ twiddle4 (unsafe-fx- k 1)))))
            (if (= sign fft-forward)
                (begin
                  ;; Forward transform
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (imag-part tw1))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (imag-part tw2))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (imag-part tw3))
                  (set! w4-real (real-part tw4))
                  (set! w4-imag (imag-part tw4)))
                (begin
                  ;; Backward transform : w -> conjugate(w)
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (- (imag-part tw1)))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (- (imag-part tw2)))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (- (imag-part tw3)))
                  (set! w4-real (real-part tw4))
                  (set! w4-imag (- (imag-part tw4)))))))
        (for ((k1 (in-range p-1)))
          (let* ((z0 (unsafe-vector-ref in i))
                 (z0-real (real-part z0))
                 (z0-imag (imag-part z0))
                 (z1 (unsafe-vector-ref in (unsafe-fx+ i m)))
                 (z1-real (real-part z1))
                 (z1-imag (imag-part z1))
                 (z2 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 2 m))))
                 (z2-real (real-part z2))
                 (z2-imag (imag-part z2))
                 (z3 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 3 m))))
                 (z3-real (real-part z3))
                 (z3-imag (imag-part z3))
                 (z4 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 4 m))))
                 (z4-real (real-part z4))
                 (z4-imag (imag-part z4)))
            (with-float (z0-real z0-imag z1-real z1-imag
                         z2-real z2-imag z3-real z3-imag
                         z4-real z4-imag)
              ;; Compute x = W(5) z          
              (let* (;; t1 = z1 + z4
                     (t1-real (unsafe-fl+ z1-real z4-real))
                     (t1-imag (unsafe-fl+ z1-imag z4-imag))
                     ;; t2 = z2 + z3
                     (t2-real (unsafe-fl+ z2-real z3-real))
                     (t2-imag (unsafe-fl+ z2-imag z3-imag))
                     ;; t3 = z1 - z4
                     (t3-real (unsafe-fl- z1-real z4-real))
                     (t3-imag (unsafe-fl- z1-imag z4-imag))
                     ;; t4 = z2 - z3
                     (t4-real (unsafe-fl- z2-real z3-real))
                     (t4-imag (unsafe-fl- z2-imag z3-imag))
                     ;; t5 = t1 + t2
                     (t5-real (unsafe-fl+ t1-real t2-real))
                     (t5-imag (unsafe-fl+ t1-imag t2-imag))
                     ;; t6 = (sqrt(5)/4)(t1 - t2)
                     (t6-real (unsafe-fl* sqrt-5-by-4 (unsafe-fl- t1-real t2-real)))
                     (t6-imag (unsafe-fl* sqrt-5-by-4 (unsafe-fl- t1-imag t2-imag)))
                     ;; t7 = z0 - ((t5)/4)
                     (t7-real (unsafe-fl- z0-real (unsafe-fl/ t5-real 4.0)))
                     (t7-imag (unsafe-fl- z0-imag (unsafe-fl/ t5-imag 4.0)))
                     ;; t8 = t7 + t6
                     (t8-real (unsafe-fl+ t7-real t6-real))
                     (t8-imag (unsafe-fl+ t7-imag t6-imag))
                     ;; t9 = t7 - t6
                     (t9-real (unsafe-fl- t7-real t6-real))
                     (t9-imag (unsafe-fl- t7-imag t6-imag))
                     ;; t10 = (+/-) sin(2 pi/5) t3 + sin(2 pi/10) t4
                     (t10-real (* sign (unsafe-fl+ (unsafe-fl* sin-2pi-by-5 t3-real)
                                                   (unsafe-fl* sin-2pi-by-10 t4-real))))
                     (t10-imag (* sign (unsafe-fl+ (unsafe-fl* sin-2pi-by-5 t3-imag)
                                                   (unsafe-fl* sin-2pi-by-10 t4-imag))))
                     ;; t11 = (+/-) sin(2 pi/10) t3 - sin(2 pi/5) t4
                     (t11-real (* sign (unsafe-fl- (unsafe-fl* sin-2pi-by-10 t3-real)
                                                   (unsafe-fl* sin-2pi-by-5 t4-real))))
                     (t11-imag (* sign (unsafe-fl- (unsafe-fl* sin-2pi-by-10 t3-imag)
                                                   (unsafe-fl* sin-2pi-by-5 t4-imag))))
                     ;; x0 = z0 + t5
                     (x0-real (unsafe-fl+ z0-real t5-real))
                     (x0-imag (unsafe-fl+ z0-imag t5-imag))
                     ;; x1 = t8 + i t10
                     (x1-real (unsafe-fl- t8-real t10-imag))
                     (x1-imag (unsafe-fl+ t8-imag t10-real))
                     ;; x2 = t9 + i t11
                     (x2-real (unsafe-fl- t9-real t11-imag))
                     (x2-imag (unsafe-fl+ t9-imag t11-real))
                     ;; x3 = t9 - i t11
                     (x3-real (unsafe-fl+ t9-real t11-imag))
                     (x3-imag (unsafe-fl- t9-imag t11-real))
                     ;; x4 = t8 - i t10
                     (x4-real (unsafe-fl+ t8-real t10-imag))
                     (x4-imag (unsafe-fl- t8-imag t10-real)))
                ;; Apply twiddle factors
                ;; to0 = 1 * x0
                (vector-set!
                 out j (make-rectangular x0-real x0-imag))
                ;; to1 = w1 * x1
                (vector-set!
                 out (unsafe-fx+ j p-1)
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w1-real x1-real)
                              (unsafe-fl* w1-imag x1-imag))
                  (unsafe-fl+ (unsafe-fl* w1-real x1-imag)
                              (unsafe-fl* w1-imag x1-real))))
                ;; to2 = w2 * x2
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 2 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w2-real x2-real)
                              (unsafe-fl* w2-imag x2-imag))
                  (unsafe-fl+ (unsafe-fl* w2-real x2-imag)
                              (unsafe-fl* w2-imag x2-real))))
                ;; to3 = w3 * x3
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 3 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w3-real x3-real)
                              (unsafe-fl* w3-imag x3-imag))
                  (unsafe-fl+ (unsafe-fl* w3-real x3-imag)
                              (unsafe-fl* w3-imag x3-real))))
                ;; to4 = w4 * x4
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 4 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w4-real x4-real)
                              (unsafe-fl* w4-imag x4-imag))
                  (unsafe-fl+ (unsafe-fl* w4-real x4-imag)
                              (unsafe-fl* w4-imag x4-real))))))
            (set! i (unsafe-fx+ i 1))
            (set! j (unsafe-fx+ j 1))))
        (set! j (unsafe-fx+ j jump))))))

;;; (fft-complex-pass-6 in out sign product n trig
;;;                     twiddle1 twiddle2 twiddle3
;;;                     twiddle4 twiddle5) -> void
;;;   in : (vectorof complex?)
;;;   out : (vectorof complex?)
;;;   sign : (one-of/c -1.0 1.0)
;;;   product : (and/c exact-positive-integer? fixnum?)
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   trig : (vectorof complex?)
;;;   twiddle1 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle2 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle3 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle4 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle5 : (and/c exact-nonnegative-integer? fixnum?)
(define (fft-complex-pass-6 in out sign product n trig
                            twiddle1 twiddle2 twiddle3
                            twiddle4 twiddle5)
  (let* ((i 0)
         (j 0)
         (factor 6)
         (m (unsafe-fxquotient n factor))
         (q (unsafe-fxquotient n product))
         (p-1 (unsafe-fxquotient product factor))
         (jump (unsafe-fx* (unsafe-fx- factor 1) p-1))
         (tau (unsafe-fl/ (unsafe-flsqrt 3.0) 2.0)))
    (for ((k (in-range q)))
      (let ((w1-real 1.0)
            (w1-imag 0.0)
            (w2-real 1.0)
            (w2-imag 0.0)
            (w3-real 1.0)
            (w3-imag 0.0)
            (w4-real 1.0)
            (w4-imag 0.0)
            (w5-real 1.0)
            (w5-imag 0.0))
        (unless (unsafe-fx= k 0)
          (let ((tw1 (unsafe-vector-ref trig (unsafe-fx+ twiddle1 (unsafe-fx- k 1))))
                (tw2 (unsafe-vector-ref trig (unsafe-fx+ twiddle2 (unsafe-fx- k 1))))
                (tw3 (unsafe-vector-ref trig (unsafe-fx+ twiddle3 (unsafe-fx- k 1))))
                (tw4 (unsafe-vector-ref trig (unsafe-fx+ twiddle4 (unsafe-fx- k 1))))
                (tw5 (unsafe-vector-ref trig (unsafe-fx+ twiddle5 (unsafe-fx- k 1)))))
            (if (= sign fft-forward)
                (begin
                  ;; Forward transform
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (imag-part tw1))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (imag-part tw2))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (imag-part tw3))
                  (set! w4-real (real-part tw4))
                  (set! w4-imag (imag-part tw4))
                  (set! w5-real (real-part tw5))
                  (set! w5-imag (imag-part tw5)))
                (begin
                  ;; Backward transform : w -> conjugate(w)
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (- (imag-part tw1)))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (- (imag-part tw2)))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (- (imag-part tw3)))
                  (set! w4-real (real-part tw4))
                  (set! w4-imag (- (imag-part tw4)))
                  (set! w5-real (real-part tw5))
                  (set! w5-imag (- (imag-part tw5)))))))
        (for ((k1 (in-range p-1)))
          (let* ((z0 (unsafe-vector-ref in i))
                 (z0-real (real-part z0))
                 (z0-imag (imag-part z0))
                 (z1 (unsafe-vector-ref in (unsafe-fx+ i m)))
                 (z1-real (real-part z1))
                 (z1-imag (imag-part z1))
                 (z2 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 2 m))))
                 (z2-real (real-part z2))
                 (z2-imag (imag-part z2))
                 (z3 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 3 m))))
                 (z3-real (real-part z3))
                 (z3-imag (imag-part z3))
                 (z4 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 4 m))))
                 (z4-real (real-part z4))
                 (z4-imag (imag-part z4))
                 (z5 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 5 m))))
                 (z5-real (real-part z5))
                 (z5-imag (imag-part z5)))
            (with-float (z0-real z0-imag z1-real z1-imag
                         z2-real z2-imag z3-real z3-imag
                         z4-real z4-imag z5-real z5-imag)
              ;; Compute x = W(6) z      
              ;; w(6) is a combination of sums and differences of w(3) acting on
              ;; the even and add elements of z.
              (let* (;; ta1 = z2 + z4
                     (ta1-real (unsafe-fl+ z2-real z4-real))
                     (ta1-imag (unsafe-fl+ z2-imag z4-imag))
                     ;; ta2 = z0 - ta1/2
                     (ta2-real (unsafe-fl- z0-real (unsafe-fl/ ta1-real 2.0)))
                     (ta2-imag (unsafe-fl- z0-imag (unsafe-fl/ ta1-imag 2.0)))
                     ;; ta3 = (+/-) sin(pi/3)*(z2-z4)
                     (ta3-real (unsafe-fl* (* sign tau) (unsafe-fl- z2-real z4-real)))
                     (ta3-imag (unsafe-fl* (* sign tau) (unsafe-fl- z2-imag z4-imag)))
                     ;; a0 = z0 + ta1
                     (a0-real (unsafe-fl+ z0-real ta1-real))
                     (a0-imag (unsafe-fl+ z0-imag ta1-imag))
                     ;; a1 = ta2 + i ta3
                     (a1-real (unsafe-fl- ta2-real ta3-imag))
                     (a1-imag (unsafe-fl+ ta2-imag ta3-real))
                     ;; a2 = ta2 - i ta3
                     (a2-real (unsafe-fl+ ta2-real ta3-imag))
                     (a2-imag (unsafe-fl- ta2-imag ta3-real))
                     ;; tb1 = z5 + z1
                     (tb1-real (unsafe-fl+ z5-real z1-real))
                     (tb1-imag (unsafe-fl+ z5-imag z1-imag))
                     ;; tb2 = z3 - tb1/2
                     (tb2-real (unsafe-fl- z3-real (unsafe-fl/ tb1-real 2.0)))
                     (tb2-imag (unsafe-fl- z3-imag (unsafe-fl/ tb1-imag 2.0)))
                     ;; tb3 = (+/-) sin(pi/3)*(z5-z1)
                     (tb3-real (unsafe-fl* (* sign tau) (unsafe-fl- z5-real z1-real)))
                     (tb3-imag (unsafe-fl* (* sign tau) (unsafe-fl- z5-imag z1-imag)))
                     ;; b0 = z3 + tb1
                     (b0-real (unsafe-fl+ z3-real tb1-real))
                     (b0-imag (unsafe-fl+ z3-imag tb1-imag))
                     ;; b1 = tb2 + i tb3
                     (b1-real (unsafe-fl- tb2-real tb3-imag))
                     (b1-imag (unsafe-fl+ tb2-imag tb3-real))
                     ;; b2 = tb2 - i tb3
                     (b2-real (unsafe-fl+ tb2-real tb3-imag))
                     (b2-imag (unsafe-fl- tb2-imag tb3-real))
                     ;; x0 = a0 + b0
                     (x0-real (unsafe-fl+ a0-real b0-real))
                     (x0-imag (unsafe-fl+ a0-imag b0-imag))
                     ;; x4 = a1 + b1
                     (x4-real (unsafe-fl+ a1-real b1-real))
                     (x4-imag (unsafe-fl+ a1-imag b1-imag))
                     ;; x2 = a2 + b2
                     (x2-real (unsafe-fl+ a2-real b2-real))
                     (x2-imag (unsafe-fl+ a2-imag b2-imag))
                     ;; x3 = a0 - b0
                     (x3-real (unsafe-fl- a0-real b0-real))
                     (x3-imag (unsafe-fl- a0-imag b0-imag))
                     ;; x1 = a1 - b1
                     (x1-real (unsafe-fl- a1-real b1-real))
                     (x1-imag (unsafe-fl- a1-imag b1-imag))
                     ;; x5 = a2 - b2
                     (x5-real (unsafe-fl- a2-real b2-real))
                     (x5-imag (unsafe-fl- a2-imag b2-imag)))
                ;; Apply twiddle factors
                ;; to0 = 1 * x0
                (vector-set!
                 out j (make-rectangular x0-real x0-imag))
                ;; to1 = w1 * x1
                (vector-set!
                 out (unsafe-fx+ j p-1)
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w1-real x1-real)
                              (unsafe-fl* w1-imag x1-imag))
                  (unsafe-fl+ (unsafe-fl* w1-real x1-imag)
                              (unsafe-fl* w1-imag x1-real))))
                ;; to2 = w2 * x2
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 2 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w2-real x2-real)
                              (unsafe-fl* w2-imag x2-imag))
                  (unsafe-fl+ (unsafe-fl* w2-real x2-imag)
                              (unsafe-fl* w2-imag x2-real))))
                ;; to3 = w3 * x3
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 3 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w3-real x3-real)
                              (unsafe-fl* w3-imag x3-imag))
                  (unsafe-fl+ (unsafe-fl* w3-real x3-imag)
                              (unsafe-fl* w3-imag x3-real))))
                ;; to4 = w4 * x4
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 4 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w4-real x4-real)
                              (unsafe-fl* w4-imag x4-imag))
                  (unsafe-fl+ (unsafe-fl* w4-real x4-imag)
                              (unsafe-fl* w4-imag x4-real))))
                ;; to5 = w5 * x5
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 5 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w5-real x5-real)
                              (unsafe-fl* w5-imag x5-imag))
                  (unsafe-fl+ (unsafe-fl* w5-real x5-imag)
                              (unsafe-fl* w5-imag x5-real))))))
            (set! i (unsafe-fx+ i 1))
            (set! j (unsafe-fx+ j 1))))
        (set! j (unsafe-fx+ j jump))))))

;;; (fft-complex-pass-7 in out sign product n trig
;;;                     twiddle1 twiddle2 twiddle3
;;;                     twiddle4 twiddle5 twiddle6) -> void
;;;   in : (vectorof complex?)
;;;   out : (vectorof complex?)
;;;   sign : (one-of/c -1.0 1.0)
;;;   product : (and/c exact-positive-integer? fixnum?)
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   trig : (vectorof complex?)
;;;   twiddle1 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle2 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle3 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle4 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle5 : (and/c exact-nonnegative-integer? fixnum?)
;;;   twiddle6 : (and/c exact-nonnegative-integer? fixnum?)
(define (fft-complex-pass-7 in out sign product n trig
                            twiddle1 twiddle2 twiddle3
                            twiddle4 twiddle5 twiddle6)
  (define c1 (unsafe-flcos (unsafe-fl/ (unsafe-fl* 1.0 2*pi) 7.0)))
  (define c2 (unsafe-flcos (unsafe-fl/ (unsafe-fl* 2.0 2*pi) 7.0)))
  (define c3 (unsafe-flcos (unsafe-fl/ (unsafe-fl* 3.0 2*pi) 7.0)))
  (define s1 (unsafe-flsin (unsafe-fl/ (unsafe-fl* 1.0 2*pi) 7.0)))
  (define s2 (unsafe-flsin (unsafe-fl/ (unsafe-fl* 2.0 2*pi) 7.0)))
  (define s3 (unsafe-flsin (unsafe-fl/ (unsafe-fl* 3.0 2*pi) 7.0)))
  (let* ((i 0)
         (j 0)
         (factor 7)
         (m (unsafe-fxquotient n factor))
         (q (unsafe-fxquotient n product))
         (p-1 (unsafe-fxquotient product factor))
         (jump (unsafe-fx* (unsafe-fx- factor 1) p-1))
         (tau (unsafe-fl/ (unsafe-flsqrt 3.0) 2.0)))
    (for ((k (in-range q)))
      (let ((w1-real 1.0)
            (w1-imag 0.0)
            (w2-real 1.0)
            (w2-imag 0.0)
            (w3-real 1.0)
            (w3-imag 0.0)
            (w4-real 1.0)
            (w4-imag 0.0)
            (w5-real 1.0)
            (w5-imag 0.0)
            (w6-real 1.0)
            (w6-imag 0.0))
        (unless (unsafe-fx= k 0)
          (let ((tw1 (unsafe-vector-ref trig (unsafe-fx+ twiddle1 (unsafe-fx- k 1))))
                (tw2 (unsafe-vector-ref trig (unsafe-fx+ twiddle2 (unsafe-fx- k 1))))
                (tw3 (unsafe-vector-ref trig (unsafe-fx+ twiddle3 (unsafe-fx- k 1))))
                (tw4 (unsafe-vector-ref trig (unsafe-fx+ twiddle4 (unsafe-fx- k 1))))
                (tw5 (unsafe-vector-ref trig (unsafe-fx+ twiddle5 (unsafe-fx- k 1))))
                (tw6 (unsafe-vector-ref trig (unsafe-fx+ twiddle6 (unsafe-fx- k 1)))))
            (if (= sign fft-forward)
                (begin
                  ;; Forward transform
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (imag-part tw1))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (imag-part tw2))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (imag-part tw3))
                  (set! w4-real (real-part tw4))
                  (set! w4-imag (imag-part tw4))
                  (set! w5-real (real-part tw5))
                  (set! w5-imag (imag-part tw5))
                  (set! w6-real (real-part tw6))
                  (set! w6-imag (imag-part tw6)))
                (begin
                  ;; Backward transform : w -> conjugate(w)
                  (set! w1-real (real-part tw1))
                  (set! w1-imag (- (imag-part tw1)))
                  (set! w2-real (real-part tw2))
                  (set! w2-imag (- (imag-part tw2)))
                  (set! w3-real (real-part tw3))
                  (set! w3-imag (- (imag-part tw3)))
                  (set! w4-real (real-part tw4))
                  (set! w4-imag (- (imag-part tw4)))
                  (set! w5-real (real-part tw5))
                  (set! w5-imag (- (imag-part tw5)))
                  (set! w6-real (real-part tw6))
                  (set! w6-imag (- (imag-part tw6)))))))
        (for ((k1 (in-range p-1)))
          (let* ((z0 (unsafe-vector-ref in i))
                 (z0-real (real-part z0))
                 (z0-imag (imag-part z0))
                 (z1 (unsafe-vector-ref in (unsafe-fx+ i m)))
                 (z1-real (real-part z1))
                 (z1-imag (imag-part z1))
                 (z2 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 2 m))))
                 (z2-real (real-part z2))
                 (z2-imag (imag-part z2))
                 (z3 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 3 m))))
                 (z3-real (real-part z3))
                 (z3-imag (imag-part z3))
                 (z4 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 4 m))))
                 (z4-real (real-part z4))
                 (z4-imag (imag-part z4))
                 (z5 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 5 m))))
                 (z5-real (real-part z5))
                 (z5-imag (imag-part z5))
                 (z6 (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* 6 m))))
                 (z6-real (real-part z6))
                 (z6-imag (imag-part z6)))
            (with-float (z0-real z0-imag z1-real z1-imag
                         z2-real z2-imag z3-real z3-imag
                         z4-real z4-imag z5-real z5-imag
                         z6-real z6-imag)
              ;; Compute x = W(7) z      
              (let* (;; t0 = z1 + z6
                     (t0-real (unsafe-fl+ z1-real z6-real))
                     (t0-imag (unsafe-fl+ z1-imag z6-imag))
                     ;; t1 = z1 - z6
                     (t1-real (unsafe-fl- z1-real z6-real))
                     (t1-imag (unsafe-fl- z1-imag z6-imag))
                     ;; t2 = z2 + z5
                     (t2-real (unsafe-fl+ z2-real z5-real))
                     (t2-imag (unsafe-fl+ z2-imag z5-imag))
                     ;; t3 = z2 - z5
                     (t3-real (unsafe-fl- z2-real z5-real))
                     (t3-imag (unsafe-fl- z2-imag z5-imag))
                     ;; t4 = z4 + z3
                     (t4-real (unsafe-fl+ z4-real z3-real))
                     (t4-imag (unsafe-fl+ z4-imag z3-imag))
                     ;; t5 = z4 - z3
                     (t5-real (unsafe-fl- z4-real z3-real))
                     (t5-imag (unsafe-fl- z4-imag z3-imag))
                     ;; t6 = t2 + t0
                     (t6-real (unsafe-fl+ t2-real t0-real))
                     (t6-imag (unsafe-fl+ t2-imag t0-imag))
                     ;; t7 = t5 + t3
                     (t7-real (unsafe-fl+ t5-real t3-real))
                     (t7-imag (unsafe-fl+ t5-imag t3-imag))
                     ;; b0 = z0 + t6 + t4
                     (b0-real (unsafe-fl+ z0-real (unsafe-fl+ t6-real t4-real)))
                     (b0-imag (unsafe-fl+ z0-imag (unsafe-fl+ t6-imag t4-imag)))
                     ;; b1 = ((cos(2pi/7) + cos(4pi/7) + cos(6pi/7))/3-1) (t6 + t4)
                     (cb1 (unsafe-fl- (unsafe-fl/ (unsafe-fl+ (unsafe-fl+ c1 c2) c3) 3.0) 1.0))
                     (b1-real (unsafe-fl* cb1 (unsafe-fl+ t6-real t4-real)))
                     (b1-imag (unsafe-fl* cb1 (unsafe-fl+ t6-imag t4-imag)))
                     ;; b2 = ((2*cos(2pi/7) - cos(4pi/7) - cos(6pi/7))/3) (t0 - t4)
                     (cb2 (unsafe-fl/ (unsafe-fl- (unsafe-fl- (unsafe-fl* 2.0 c1) c2) c3) 3.0))
                     (b2-real (unsafe-fl* cb2 (unsafe-fl- t0-real t4-real)))
                     (b2-imag (unsafe-fl* cb2 (unsafe-fl- t0-imag t4-imag)))
                     ;; b3 = ((cos(2pi/7) - 2*cos(4pi/7) + cos(6pi/7))/3) (t4 - t2)
                     (cb3 (unsafe-fl/ (unsafe-fl+ (unsafe-fl- c1 (unsafe-fl* 2.0 c2)) c3) 3.0))
                     (b3-real (unsafe-fl* cb3 (unsafe-fl- t4-real t2-real)))
                     (b3-imag (unsafe-fl* cb3 (unsafe-fl- t4-imag t2-imag)))
                     ;; b4 = ((cos(2pi/7) + cos(4pi/7) - 2*cos(6pi/7))/3) (t2 - t0)
                     (cb4 (unsafe-fl/ (unsafe-fl- (unsafe-fl+ c1 c2) (unsafe-fl* 2.0 c3)) 3.0))
                     (b4-real (unsafe-fl* cb4 (unsafe-fl- t2-real t0-real)))
                     (b4-imag (unsafe-fl* cb4 (unsafe-fl- t2-imag t0-imag)))
                     ;; b5 = sign * ((sin(2pi/7) + sin(4pi/7) - sin(6pi/7))/3) (t7 + t1)
                     (cb5 (* (- sign) (unsafe-fl/ (unsafe-fl- (unsafe-fl+ s1 s2) s3) 3.0)))
                     (b5-real (unsafe-fl* cb5 (unsafe-fl+ t7-real t1-real)))
                     (b5-imag (unsafe-fl* cb5 (unsafe-fl+ t7-imag t1-imag)))
                     ;; b6 = sign * ((2sin(2pi/7) - sin(4pi/7) + sin(6pi/7))/3) (t1 - t5)
                     (cb6 (* (- sign) (unsafe-fl/ (unsafe-fl+ (unsafe-fl- (unsafe-fl* 2.0 s1) s2) s3) 3.0)))
                     (b6-real (unsafe-fl* cb6 (unsafe-fl- t1-real t5-real)))
                     (b6-imag (unsafe-fl* cb6 (unsafe-fl- t1-imag t5-imag)))
                     ;; b7 = sign * ((sin(2pi/7) - 2sin(4pi/7) - sin(6pi/7))/3) (t5 - t3)
                     (cb7 (* (- sign) (unsafe-fl/ (unsafe-fl- (unsafe-fl- s1 (unsafe-fl* 2.0 s2)) s3) 3.0)))
                     (b7-real (unsafe-fl* cb7 (unsafe-fl- t5-real t3-real)))
                     (b7-imag (unsafe-fl* cb7 (unsafe-fl- t5-imag t3-imag)))
                     ;; b8 = sign * ((sin(2pi/7) + sin(4pi/7) + 2sin(6pi/7))/3) (t3 - t1)
                     (cb8 (* (- sign) (unsafe-fl/ (unsafe-fl+ (unsafe-fl+ s1 s2) (unsafe-fl* 2.0 s3)) 3.0)))
                     (b8-real (unsafe-fl* cb8 (unsafe-fl- t3-real t1-real)))
                     (b8-imag (unsafe-fl* cb8 (unsafe-fl- t3-imag t1-imag)))
                     ;; T0 = b0 + b1
                     (T0-real (unsafe-fl+ b0-real b1-real))
                     (T0-imag (unsafe-fl+ b0-imag b1-imag))
                     ;; T1 = b2 + b3
                     (T1-real (unsafe-fl+ b2-real b3-real))
                     (T1-imag (unsafe-fl+ b2-imag b3-imag))
                     ;; T2 = b4 - b3
                     (T2-real (unsafe-fl- b4-real b3-real))
                     (T2-imag (unsafe-fl- b4-imag b3-imag))
                     ;; T3 = -b2 - b4
                     (T3-real (unsafe-fl- (- b2-real) b4-real))
                     (T3-imag (unsafe-fl- (- b2-imag) b4-imag))
                     ;; T4 = b6 + b7
                     (T4-real (unsafe-fl+ b6-real b7-real))
                     (T4-imag (unsafe-fl+ b6-imag b7-imag))
                     ;; T5 = b8 - b7
                     (T5-real (unsafe-fl- b8-real b7-real))
                     (T5-imag (unsafe-fl- b8-imag b7-imag))
                     ;; T6 = -b8 - b6
                     (T6-real (unsafe-fl- (- b8-real) b6-real))
                     (T6-imag (unsafe-fl- (- b8-imag) b6-imag))
                     ;; T7 = T0 + T1
                     (T7-real (unsafe-fl+ T0-real T1-real))
                     (T7-imag (unsafe-fl+ T0-imag T1-imag))
                     ;; T8 = T0 + T2
                     (T8-real (unsafe-fl+ T0-real T2-real))
                     (T8-imag (unsafe-fl+ T0-imag T2-imag))
                     ;; T9 = T0 + T3
                     (T9-real (unsafe-fl+ T0-real T3-real))
                     (T9-imag (unsafe-fl+ T0-imag T3-imag))
                     ;; T10 = T4 + b5
                     (T10-real (unsafe-fl+ T4-real b5-real))
                     (T10-imag (unsafe-fl+ T4-imag b5-imag))
                     ;; T11 = T5 + b5
                     (T11-real (unsafe-fl+ T5-real b5-real))
                     (T11-imag (unsafe-fl+ T5-imag b5-imag))
                     ;; T12 = T6 + b5
                     (T12-real (unsafe-fl+ T6-real b5-real))
                     (T12-imag (unsafe-fl+ T6-imag b5-imag))
                     ;; x0 = b0
                     (x0-real b0-real)
                     (x0-imag b0-imag)
                     ;; x1 = T7 = i T10
                     (x1-real (unsafe-fl+ T7-real T10-imag))
                     (x1-imag (unsafe-fl- T7-imag T10-real))
                     ;; x2 = T9 - i T12
                     (x2-real (unsafe-fl+ T9-real T12-imag))
                     (x2-imag (unsafe-fl- T9-imag T12-real))
                     ;; x3 = T8 + i T11
                     (x3-real (unsafe-fl- T8-real T11-imag))
                     (x3-imag (unsafe-fl+ T8-imag T11-real))
                     ;; x4 = T8 - i T11
                     (x4-real (unsafe-fl+ T8-real T11-imag))
                     (x4-imag (unsafe-fl- T8-imag T11-real))
                     ;; x5 = T9 + i T12
                     (x5-real (unsafe-fl- T9-real T12-imag))
                     (x5-imag (unsafe-fl+ T9-imag T12-real))
                     ;; x6 = T7 + i T10
                     (x6-real (unsafe-fl- T7-real T10-imag))
                     (x6-imag (unsafe-fl+ T7-imag T10-real)))
                ;; Apply twiddle factors
                ;; to0 = 1 * x0
                (vector-set!
                 out j (make-rectangular x0-real x0-imag))
                ;; to1 = w1 * x1
                (vector-set!
                 out (unsafe-fx+ j p-1)
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w1-real x1-real)
                              (unsafe-fl* w1-imag x1-imag))
                  (unsafe-fl+ (unsafe-fl* w1-real x1-imag)
                              (unsafe-fl* w1-imag x1-real))))
                ;; to2 = w2 * x2
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 2 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w2-real x2-real)
                              (unsafe-fl* w2-imag x2-imag))
                  (unsafe-fl+ (unsafe-fl* w2-real x2-imag)
                              (unsafe-fl* w2-imag x2-real))))
                ;; to3 = w3 * x3
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 3 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w3-real x3-real)
                              (unsafe-fl* w3-imag x3-imag))
                  (unsafe-fl+ (unsafe-fl* w3-real x3-imag)
                              (unsafe-fl* w3-imag x3-real))))
                ;; to4 = w4 * x4
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 4 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w4-real x4-real)
                              (unsafe-fl* w4-imag x4-imag))
                  (unsafe-fl+ (unsafe-fl* w4-real x4-imag)
                              (unsafe-fl* w4-imag x4-real))))
                ;; to5 = w5 * x5
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 5 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w5-real x5-real)
                              (unsafe-fl* w5-imag x5-imag))
                  (unsafe-fl+ (unsafe-fl* w5-real x5-imag)
                              (unsafe-fl* w5-imag x5-real))))
                ;; to6 = w6 * x6
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* 6 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w6-real x6-real)
                              (unsafe-fl* w6-imag x6-imag))
                  (unsafe-fl+ (unsafe-fl* w6-real x6-imag)
                              (unsafe-fl* w6-imag x6-real))))))
            (set! i (unsafe-fx+ i 1))
            (set! j (unsafe-fx+ j 1))))
        (set! j (unsafe-fx+ j jump))))))

;;; (fft-complex-pass-n in out sign factor product n trig
;;;                     twiddle) -> void
;;;   in : (vectorof complex?)
;;;   out : (vectorof complex?)
;;;   sign : (one-of/c -1.0 1.0)
;;;   factor : (and/c exact-positive-integer? fixnum?)
;;;   product : (and/c exact-positive-integer? fixnum?)
;;;   n : (and/c exact-positive-integer? fixnum?)
;;;   trig : (vectorof complex?)
;;;   twiddle : (and/c exact-nonnegative-integer? fixnum?)
(define (fft-complex-pass-n in out sign factor product n
                            trig twiddle)
  (let* ((i 0)
         (j 0)
         (m (unsafe-fxquotient n factor))
         (q (unsafe-fxquotient n product))
         (p-1 (unsafe-fxquotient product factor))
         (jump (unsafe-fx* (unsafe-fx- factor 1) p-1)))
    (for ((i (in-range m)))
      (unsafe-vector-set! out i (unsafe-vector-ref in i)))
    (for ((e (in-range 1 (unsafe-fx+ (unsafe-fxquotient
                                      (unsafe-fx- factor 1) 2) 1))))
      (for ((i (in-range 0 m)))
        (let* ((idx (unsafe-fx+ i (unsafe-fx* e m)))
               (idxc (unsafe-fx+ i (unsafe-fx* (unsafe-fx- factor e) m)))
               (z0 (unsafe-vector-ref in idx))
               (z1 (unsafe-vector-ref in idxc)))
          (unsafe-vector-set! out idx (+ z0 z1))
          (unsafe-vector-set! out idxc (- z0 z1)))))
    ;; e = 0
    (for ((i (in-range m)))
      (unsafe-vector-set! in i (unsafe-vector-ref out i)))
    (for ((e1 (in-range 1 (unsafe-fx+ (unsafe-fxquotient
                                       (unsafe-fx- factor 1) 2) 1))))
      (for ((i (in-range m)))
        (let ((z0 (unsafe-vector-ref in i))
              (z1 (unsafe-vector-ref out (unsafe-fx+ i (unsafe-fx* e1 m)))))
          (unsafe-vector-set! in i (+ z0 z1)))))
    (for ((e (in-range 1 (unsafe-fx+ (unsafe-fxquotient
                                      (unsafe-fx- factor 1) 2) 1))))
      (let ((idx (unsafe-fx* e q))
            (idx-step (unsafe-fx* e q))
            (w-real 1.0)
            (w-imag 0.0)
            (em (unsafe-fx* e m))
            (ecm (unsafe-fx* (unsafe-fx- factor e) m)))
        (for ((i (in-range m)))
          (unsafe-vector-set! in (unsafe-fx+ i em) (unsafe-vector-ref out i))
          (unsafe-vector-set! in (unsafe-fx+ i ecm) (unsafe-vector-ref out i)))
        (for ((e1 (in-range 1 (unsafe-fx+ (unsafe-fxquotient
                                           (unsafe-fx- factor 1) 2) 1))))
          (if (unsafe-fx= idx 0)
              (begin
                (set! w-real 1.0)
                (set! w-imag 0.0))
              (let ((tw (unsafe-vector-ref trig (unsafe-fx+
                                                 twiddle (unsafe-fx- idx 1)))))
                (if (= sign fft-forward)
                    (begin
                      (set! w-real (real-part tw))
                      (set! w-imag (imag-part tw)))
                    (begin
                      (set! w-real (real-part tw))
                      (set! w-imag (- (imag-part tw)))))))
          (for ((i (in-range m)))
            (let* ((xp (unsafe-vector-ref
                        out (unsafe-fx+ i (unsafe-fx* e1 m))))
                   (xp-real (real-part xp))
                   (xp-imag (imag-part xp))
                   (xm (unsafe-vector-ref
                        out (unsafe-fx+ i (unsafe-fx*
                                           (unsafe-fx- factor e1) m))))
                   (xm-real (real-part xm))
                   (xm-imag (imag-part xm)))
              (with-float (xp-real xp-imag xm-real xm-imag)
                (let* ((ap (unsafe-fl* w-real xp-real))
                       (am (unsafe-fl* w-imag xm-imag))
                       (sum-real (unsafe-fl- ap am))
                       (sumc-real (unsafe-fl+ ap am))
                       (bp (unsafe-fl* w-real xp-imag))
                       (bm (unsafe-fl* w-imag xm-real))
                       (sum-imag (unsafe-fl+ bp bm))
                       (sumc-imag (unsafe-fl- bp bm))
                       (z0 (unsafe-vector-ref in (unsafe-fx+ i em)))
                       (z0-real (real-part z0))
                       (z0-imag (imag-part z0))
                       (z1 (unsafe-vector-ref in (unsafe-fx+ i ecm)))
                       (z1-real (real-part z1))
                       (z1-imag (imag-part z1)))
                  (with-float (z0-real z0-imag z1-real z1-imag)
                    (unsafe-vector-set!
                     in (unsafe-fx+ i em)
                     (make-rectangular (unsafe-fl+ z0-real sum-real)
                                       (unsafe-fl+ z0-imag sum-imag)))
                    (unsafe-vector-set!
                     in (unsafe-fx+ i ecm)
                     (make-rectangular (unsafe-fl+ z1-real sumc-real)
                                       (unsafe-fl+ z1-imag sumc-imag))))))))
          (set! idx (unsafe-fx+ idx idx-step))
          (set! idx (unsafe-fxmodulo idx (unsafe-fx* factor q))))))
    (set! i 0)
    (set! j 0)
    ;; k = 0
    (for ((k1 (in-range p-1)))
      (unsafe-vector-set! out k1 (unsafe-vector-ref in k1)))
    (for ((e1 (in-range 1 factor)))
      (for ((k1 (in-range p-1)))
        (unsafe-vector-set!
         out (unsafe-fx+ k1 (unsafe-fx* e1 p-1))
         (unsafe-vector-ref in (unsafe-fx+ k1 (unsafe-fx* e1 m))))))
    (set! i p-1)
    (set! j product)
    (for ((k (in-range 1 q)))
      (for ((k1 (in-range p-1)))
        (unsafe-vector-set! out j (unsafe-vector-ref in i))
        (set! i (unsafe-fx+ i 1))
        (set! j (unsafe-fx+ j 1)))
      (set! j (unsafe-fx+ j jump)))
    (set! i p-1)
    (set! j product)
    (for ((k (in-range 1 q)))
      (for ((k1 (in-range p-1)))
        (for ((e1 (in-range 1 factor)))
          (let* ((x (unsafe-vector-ref in (unsafe-fx+ i (unsafe-fx* e1 m))))
                 (x-real (real-part x))
                 (x-imag (imag-part x)))
            (with-float (x-real x-imag)
              (let ((w-real 1.0)
                    (w-imag 0.0))
                (let ((tw (unsafe-vector-ref
                           trig
                           (unsafe-fx+ twiddle (unsafe-fx-
                                                (unsafe-fx+
                                                 (unsafe-fx*
                                                  (unsafe-fx- e1 1) q) k) 1)))))
                  (if (= sign fft-forward)
                      (begin
                        (set! w-real (real-part tw))
                        (set! w-imag (imag-part tw)))
                      (begin
                        (set! w-real (real-part tw))
                        (set! w-imag (- (imag-part tw))))))
                (vector-set!
                 out (unsafe-fx+ j (unsafe-fx* e1 p-1))
                 (make-rectangular
                  (unsafe-fl- (unsafe-fl* w-real x-real)
                              (unsafe-fl* w-imag x-imag))
                  (unsafe-fl+ (unsafe-fl* w-real x-imag)
                              (unsafe-fl* w-imag x-real))))))))
        (set! i (unsafe-fx+ i 1))
        (set! j (unsafe-fx+ j 1)))
      (set! j (unsafe-fx+ j jump)))))

;;; Generic DFT

;;; (dft-complex-forward data) -> (vectorof complex?)
;;;   data : (vectorof complex?)
(define (dft-complex-forward data)
  (dft-complex-transform data fft-forward))

;;; (dft-complex-backward data) -> (vectorof complex?)
;;;   data : (vectorof complex?)
(define (dft-complex-backward data)
  (dft-complex-transform data fft-backward))

;;; (dft-complex-inverse data) -> (vectorof complex?)
;;;   data : (vectorof complex?)
(define (dft-complex-inverse data)
  (let ((result (dft-complex-backward data)))
    ;; Normalize inverse FFT with 1/n.
    (let* ((n (unsafe-vector-length result))
           (norm (unsafe-fl/ 1.0 (unsafe-fx->fl n))))
      (for ((i (in-range n)))
        (let* ((datum (unsafe-vector-ref result i))
               (datum-real (real-part datum))
               (datum-imag (imag-part datum)))
          (unsafe-vector-set!
           result i
           (make-rectangular
            (unsafe-fl* norm datum-real)
            (unsafe-fl* norm datum-imag))))))
    result))

;;; (dft-complex-transform data sign) -> (vectorof complex?)
;;;   data : (vectorof complex?)
;;;   sign : (one-of -1.0 1.0)
(define (dft-complex-transform data sign)
  (with-float (sign)
    (let* ((n (vector-length data))
           (d-theta (unsafe-fl/ (unsafe-fl* sign 2*pi)
                                (unsafe-fx->fl n))))
      (build-vector
       n
       (lambda (i)
         (let-values
             (((sum-real sum-imag exponent)
               (for/fold ((sum-real 0.0)
                          (sum-imag 0.0)
                          (exponent 0))
                         ((datum (in-vector data)))
                 (let* ((theta (unsafe-fl* d-theta (unsafe-fx->fl exponent)))
                        (w-real (unsafe-flcos theta))
                        (w-imag (unsafe-flsin theta))
                        (datum-real (real-part datum))
                        (datum-imag (imag-part datum)))
                   (with-float (datum-real datum-imag)
                     (values
                      (unsafe-fl+ sum-real
                         (unsafe-fl- (unsafe-fl* w-real datum-real)
                                     (unsafe-fl* w-imag datum-imag)))
                      (unsafe-fl+ sum-imag
                         (unsafe-fl+ (unsafe-fl* w-real datum-imag)
                                     (unsafe-fl* w-imag datum-real)))
                      (unsafe-fxmodulo (unsafe-fx+ exponent i) n)))))))
           (make-rectangular sum-real sum-imag)))))))

;;; Module Contracts

(provide
 fft-forward
 fft-backward
 (rename-out
  (fft-complex-forward unchecked-fft-complex-forward)
  (fft-complex-backward unchecked-fft-complex-backward)
  (fft-complex-inverse unchecked-fft-complex-inverse)
  (fft-complex-transform unchecked-fft-complex-transform)
  (fft-complex-radix2-forward unchecked-fft-complex-radix2-forward)
  (fft-complex-radix2-backward unchecked-fft-complex-radix2-backward)
  (fft-complex-radix2-inverse unchecked-fft-complex-radix2-inverse)
  (fft-complex-radix2-transform unchecked-fft-complex-radix2-transform)
  (fft-complex-radix2-dif-forward unchecked-fft-complex-radix2-dif-forward)
  (fft-complex-radix2-dif-backward unchecked-fft-complex-radix2-dif-backward)
  (fft-complex-radix2-dif-inverse unchecked-fft-complex-radix2-dif-inverse)
  (fft-complex-radix2-dif-transform unchecked-fft-complex-radix2-dif-transform)
  (dft-complex-forward unchecked-dft-complex-forward)
  (dft-complex-backward unchecked-dft-complex-backward)
  (dft-complex-inverse unchecked-dft-complex-inverse)
  (dft-complex-transform unchecked-dft-complex-transform)))

(provide/contract
 (fft-complex-workspace?
  (-> any/c boolean?))
 (make-fft-complex-workspace
  (-> exact-positive-integer? fft-complex-workspace?))
 (fft-complex-forward
  (->* ((vectorof complex?))
       (#:workspace fft-complex-workspace?)
       void?))
 (fft-complex-backward
  (->* ((vectorof complex?))
       (#:workspace fft-complex-workspace?)
       void?))
 (fft-complex-inverse
  (->* ((vectorof complex?))
       (#:workspace fft-complex-workspace?)
       void?))
 (fft-complex-transform
  (->* ((vectorof complex?) (one-of/c -1.0 1.0))
       (#:workspace fft-complex-workspace?)
       void?))
 (fft-complex-radix2-forward
  (-> (vectorof complex?) void?))
 (fft-complex-radix2-backward
  (-> (vectorof complex?) void?))
 (fft-complex-radix2-inverse
  (-> (vectorof complex?) void?))
 (fft-complex-radix2-transform
  (-> (vectorof complex?) (one-of/c -1.0 1.0) void?))
 (fft-complex-radix2-dif-forward
  (-> (vectorof complex?) void?))
 (fft-complex-radix2-dif-backward
  (-> (vectorof complex?) void?))
 (fft-complex-radix2-dif-inverse
  (-> (vectorof complex?) void?))
 (fft-complex-radix2-dif-transform
  (-> (vectorof complex?) (one-of/c -1.0 1.0) void?))
 (dft-complex-forward
  (-> (vectorof complex?) (vectorof complex?)))
 (dft-complex-backward
  (-> (vectorof complex?) (vectorof complex?)))
 (dft-complex-inverse
  (-> (vectorof complex?) (vectorof complex?)))
 (dft-complex-transform
  (-> (vectorof complex?) (one-of/c -1.0 1.0) (vectorof complex?))))