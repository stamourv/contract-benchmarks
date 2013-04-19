;;; Science Collection
;;; special-functions/gamma-inc-imp.rkt
;;; Copyright (c) 2006-2010 M. Douglas Williams
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
;;; This file implements the gamma functions.  It is based on the Special
;;; Functions in the GNU Scientific Library (GSL).
;;;
;;; Version  Date      Description
;;; 1.0.0    02/08/06  Initial implementation. (MDW)
;;; 2.0.0    11/17/07  Added unchecked versions of functions and getting ready
;;;                    for PLT Scheme V4.0. (MDW)
;;; 3.0.0    06/09/08  Changes required for V4.0. (MDW)
;;; 3.0.1    09/13/09  Fixed errors found by the optimizer. (MDW)
;;; 4.0.0    07/03/10  Changed the header and restructured the code. (MDW)

;;; FIXME
(define (log1pmx x)
  (- (log1p x) x))

(define twopi (* 2.0 pi))

;;; The dominant part.
;;; D(a,x) = x^a e^(-x) / Gamma(a+1)
(define (gamma-inc-D a x)
  (if (< a 10.0)
      (let* ((lg (lngamma (+ a 1.0)))
             (lnr (- (* a (log x)) x lg)))
        (exp lnr))
      (let ((gstar (gammastar a))
            (ln-term 0.0)
            (term1 0.0))
        (if (< x a)
            (let ((u (/ x a)))
              (set! ln-term (+ (log u) (- u) 1.0)))
            (let ((mu (/ (- x a) a)))
              (set! ln-term (log1pmx mu))))
        (set! term1 (/ (exp (* a ln-term))
                       (sqrt (* twopi a))))
        (/ term1 gstar))))

;;; P series representation
(define (gamma-inc-P-series a x)
  (define nmax 5000)
  (let ((D (gamma-inc-D a x))
        (sum 1.0)
        (term 1.0)
        (n 1))
    (let loop ()
      (when (< n nmax)
        (set! term (* term (/ x (+ a n))))
        (set! sum (+ sum term))
        (when (>= (abs (/ term sum)) double-epsilon)
          (set! n (+ n 1))
          (loop))))
    (when (= n nmax)
      (error 'gamma-inc-P-series
             "maximum iterations exceeded"))
    (* D sum)))

;;; Q large x asymptotic
(define (gamma-inc-Q-large-x a x)
  (define nmax 5000)
  (let ((D (gamma-inc-D a x))
        (sum 1.0)
        (term 1.0)
        (last 1.0)
        (n 1))
    (let loop ()
      (when (< n nmax)
        (set! term (* term (/ (- a n) x)))
        (when (and (<= (abs (/ term last)) 1.0)
                   (>= (abs (/ term sum)) double-epsilon))
          (set! sum (+ sum term))
          (set! last term)
          (set! n (+ n 1))
          (loop))))
    (when (= n nmax)
      (error 'gamma=inc-Q-large-x
             "maximim iterations exceeded"))
    (* D (/ a x) sum)))

;;; Uniform asymptotic for x near a, a and x large.
;;; See [Temme, p. 285]
;;; FIXME: need c1 coefficient
(define (gamma-inc-Q-asymp-unif a x)
  (let* ((rta (sqrt a))
         (eps (/ (- x a) a))
         (ln-term (log1pmx eps))
         (eta (* eps (sqrt (/ (* -2.0 ln-term) (* eps eps)))))
         (erfc-val (unchecked-erfc (* eta sqrt2 rta)))
         (r 0.0)
         (c0 0.0)
         (c1 0.0))
    (if (< (abs eps) root5-double-epsilon)
        (begin
          (set! c0 (+ (/ -1.0 3.0)
                      (* eps
                         (- (/ 1.0 12.0)
                                (* eps
                                   (- (/ 23.0 540.0)
                                      (* eps
                                         (- (/ 353.0 12960.0)
                                            (* eps (/ 589.0 30240.0))))))))))
          (set! c1 0.0))
        (let ((rt-term (sqrt (/ (* -2.0 ln-term) (* eps eps)))))
          (set! c0 (/ (- 1.0 (/ 1.0 rt-term)) eps))
          (set! c1 0.0)))
    (set! r (* (/ (exp (* -0.5 a eta eta)) (* sqrt2 sqrtpi rta))
               (+ c0 (/ c1 a))))
    (+ (* 0.5 erfc) r)))

;;; Continued fraction which occurs in evaluation of Q(a,x) or
;;; Gamma(a,x).
;;;
;;;           1   (1-a)/x  1/x  (2-a)/x  2/x  (3-a)/x
;;; F(a,x) = ---- ------- ----- ------- ----- ------- ...
;;;           1 +  1 +     1 +   1 +     1 +   1 +
;;;
;;; Hans E. Plesser, 2002-01-22 (hans dot plesser at itf dot nlh dot no).
;;;
;;; Split out from gamma-inc-Q-CF() bu GJ
;;; See gamma-inc-Q-CF
(define (gamma-inc-F-CF a x)
  (define nmax 5000)
  ;; Ticket #199 - MDW
  ;;(define small (exp double-epsilon 3))
  (define small (expt double-epsilon 3))
  (let ((hn 1.0)                        ; convergent
        (cn (/ 1.0 small))
        (dn 1.0)
        (n 2))
    (let loop ()
      (when (< n nmax)
        (let ((an 0.0)
              (delta 0.0))
          (if (odd? n)
              (set! an (/ (* 0.5 (- n 1)) x))
              (set! an (/ (- (* 0.5 n) a) x)))
          (set! dn (+ 1.0 (* an dn)))
          (when (< (abs dn) small)
            (set! dn small))
          (set! cn (+ 1.0 (/ an cn)))
          (when (< (abs cn) small)
            (set! cn small))
          (set! dn (/ 1.0 dn))
          (set! delta (* cn dn))
          (set! hn (* hn delta))
          (when (>= (abs (- delta 1.0)) double-epsilon)
            (set! n (+ n 1))
            (loop)))))
    (when (= n nmax)
      (error 'gamma-inc-F-CF
             "maximum iterations exceeded"))
    hn))

;;; Continued fraction for Q.
(define (gamma-inc-Q-CF a x)
  (let ((D (gamma-inc-D a x))
        (F (gamma-inc-F-CF a x)))
    (* D (/ a x) F)))

;;; Useful for small a and x.  Handles the subtraction analytically.
(define (gamma-inc-Q-series a x)
  (define nmax 5000)
  (let* ((pg21 -2.404113806319188570799476) ; PolyGamma(2,1)
         (lnx (log x))
         (e1 (+ euler lnx))
         (c1 (- e1))
         (c2 (- (/ (* pi pi) 12.0) (* 0.5 e1 e1)))
         (c3 (+ (* e1 (- (/ (* pi pi) 12.0) (/ (* e1 e1) 6.0)))
                (/ pg21 6.0)))
         (c4 (* -0.04166666666666666667
                (+ -1.758243446661483480 lnx)
                (+ -0.764428657272716373 lnx)
                (+ 0.723980571623507657 lnx)
                (+ 4.107554191916823640 lnx)))
         (c5 (* -0.0083333333333333333
                (+ -2.06563396085715900 lnx)
                (+ -1.28459889470864700 lnx)
                (+ -0.27583535756454143 lnx)
                (+ 1.33677371336239618 lnx)
                (+ 5.17537282427561550 lnx)))
         (c6 (* -0.0013888888888888889
                (+ -2.30814336454783200 lnx)
                (+ -1.65846557706987300 lnx)
                (+ -0.88768082560020400 lnx)
                (+ 0.17043847751371778 lnx)
                (+ 1.92135970115863890 lnx)
                (+ 6.22578557795474900 lnx)))
         (c7 (* -0.00019841269841269841
                (+ -2.5078657901291800 lnx)
                (+ -1.9478900888958200 lnx)
                (+ -1.3194837322612730 lnx)
                (+ -0.5281322700249279 lnx)
                (+ 0.5913834939078759 lnx)
                (+ 2.4876819633378140 lnx)
                (+ 7.2648160783762400 lnx)))
         (c8 (* -0.00002480158730158730
                (+ -2.677341544966400 lnx)
                (+ -2.182810448271700 lnx)
                (+ -1.649350342277400 lnx)
                (+ -1.014099048290790 lnx)
                (+ -0.191366955370652 lnx)
                (+ 0.995403817918724 lnx)
                (+ 3.041323283529310 lnx)
                (+ 8.295966556941250 lnx)))
         (c9 (* -2.75573192239859e-6
                (+ -2.8243487670469080 lnx)
                (+ -2.3798494322701120 lnx)
                (+ -1.9143674728689960 lnx)
                (+ -1.3814529102920370 lnx)
                (+ -0.7294312810261694 lnx)
                (+ 0.1299079285269565 lnx)
                (+ 1.3873333251885240 lnx)
                (+ 3.5857258865210760 lnx)
                (+ 9.3214237073814600 lnx)))
         (c10 (* -2.75573192239859e-7
                 (+ -2.9540329644556910 lnx)
                 (+ -2.5491366926991850 lnx)
                 (+ -2.1348279229279880 lnx)
                 (+ -1.6741881076349450 lnx)
                 (+ -1.1325949616098420 lnx)
                 (+ -0.4590034650618494 lnx)
                 (+ 0.4399352987435699 lnx)
                 (+ 1.7702236517651670 lnx)
                 (+ 4.1231539047474080 lnx)
                 (+ 10.342627908148680 lnx)))
         (sum5 (+ c5
                  (* a
                     (+ c6
                        (* a
                           (+ c7
                              (* a
                                 (+ c8
                                    (* a
                                       (+ c9
                                          (* a
                                             c10)))))))))))
         (term1 (* a
                   (+ c1
                      (* a
                         (+ c2
                            (* a
                               (+ c3
                                  (* a
                                     (+ c4
                                        (* a sum5))))))))))
         (term2 0.0)
         (sum 1.0)
         (t 1.0)
         (n 1))
    (let loop ()
      (when (< n nmax)
        (set! t (* t (/ (- x) (+ n 1.0))))
        (set! sum (+ sum (* (/ (+ a 1.0) (+ a n 1.0)) t)))
        (when (>= (abs (/ t sum)) double-epsilon)
          (set! n (+ n 1))
          (loop))))
    (when (= n nmax)
      (error 'gamma-inc-Q-series
             "maximum iterations exceeded"))
    (set! term2 (* (- 1.0 term1) (/ a (+ a 1.0)) x sum))
    (+ term1 term2)))

;;; Series for small a and x, but not defined for a = 0
(define (gamma-inc-series a x)
  (let ((Q (gamma-inc-Q-series a x))
        (G (gamma a)))
    (* Q G)))

(define (gamma-inc-a-gt-0 a x)
  (let ((Q (gamma-inc-Q a x))
        (G (gamma a)))
    (* G Q)))

(define (gamma-inc-CF a x)
  (let ((F (gamma-inc-F-CF a x))
        (pre (exp (- (* (- a 1.0) (log x)) x))))
    (* F pre)))

;;; Evaluate Gamma(0,x), x > 0
(define (gamma-inc-a-0 x)
  (expint-E1 x))

(define (gamma-inc-Q a x)
  (cond ((= x 0.0)
         1.0)
        ((= a 0.0)
         0.0)
        ((<= x (+ 0.5 a))
         ;; If the series is quick, do that.  It is
         ;; robust and simple.
         (- 1.0 (gamma-inc-P-series a x)))
        ((and (>= a 1.0e06)
              (< (* (- x a) (- x a)) a))
         ;; Then try the difficult asymptotic regime.
         ;; This is the only way to do this region.
         (gamma-inc-Q-asymp-unif a x))
        ((and (< a 0.2)
              (< x 5.0))
         ;; Cancellations at small a must be handled
         ;; analytically; x should not be too big
         ;; either since the series terms grow
         ;; with x and log(x).
         (gamma-inc-Q-series a x))
        ((<= a x)
         (if (<= x 1.0e06)
             ;; Continues fraction is excellent for x >~ a.
             ;; We do not let x be too large when x > a since
             ;; it is somewhat pointless to try this there;
             ;; the function is rapidly decreasing for
             ;; x large and x > a, and it will just
             ;; underflow in that region anyway.  We
             ;; catch that case in the standard
             ;; large-x method.
             (gamma-inc-Q-CF a x)
             (gamma-inc-Q-large-x a x)))
        (else
         (if (< a (* 0.8 x))
             ;; Continued fraction again.  The convergence
             ;; is a little slower here, but that is fine.
             ;; We have to trade that off against the slow
             ;; convergence of the series, which is the
             ;; only other option.
             (gamma-inc-Q-CF a x)
             (- 1.0 (gamma-inc-P-series a x))))))

(define (gamma-inc-P a x)
  (cond ((= x 0.0)
         0.0)
        ((or (< x 20.0)
             (< x (* 0.5 a)))
         ;; Do the easy series cases.  Robust and quick.
         (gamma-inc-P-series a x))
        ((and (> a 1.0e06)
              (< (* (- x a) (- x a)) a))
         ;; Crossover region.  Note that Q and P are
         ;; roughly the same order of magnitude here,
         ;; so the subtraction is stable.
         (- 1.0 (gamma-inc-Q-asymp-unif a x)))
        ((<= a x)
         ;; Q <~ P in this area, so the
         ;; subtractions are stable.
         (let ((Q 0.0))
           (if (> a (* 0.2 x))
               (set! Q (gamma-inc-Q-CF a x))
               (set! Q (gamma-inc-Q-large-x a x)))
           (- 1.0 Q)))
        ((< (* (- x a)(- x a)) a)
         ;; This condition is meant to ensure
         ;; that Q is not very close to 1,
         ;; so the subtraction is stable.
         (let ((Q (gamma-inc-Q-CF a x)))
           (- 1.0 Q)))
        (else
         (gamma-inc-P-series a x))))

(define (gamma-inc a x)
  (cond ((= x 0.0)
         (gamma a))
        ((= a 0.0)
         (gamma-inc-a-0 x))
        ((> a 0.0)
         (gamma-inc-a-gt-0 a x))
        ((> x 0.25)
         ;; continued fraction seems to fail for x too small;
         ;; otherwise it is ok, independent of the value of |x/a|,
         ;; because of the non-oscillation in the expansion, i.e.
         ;; the CF is unconditionally convergent for a<0 and x>0
         (gamma-inc-CF a x))
        ((< (abs a) 0.5)
         (gamma-inc-series a x))
        (else
         ;; a = fa + da; da >= 0
         (let* ((fa (floor a))
                (da (- a fa))
                (g-da (if (> da 0.0)
                          (gamma-inc-a-gt-0 da x)
                          (gamma-inc-a-0 x)))
                (alpha da)
                (gax g-da))
           ;; Gamma(alpha-1,x)=1/(alpha-1)(Gamma(a,x)-x^(alpha-1)e^-x
           (let loop ()
             (when (> alpha a)
               (let ((shift (exp (+ (- x) (* (- alpha 1.0) (log x))))))
                 (set! gax (/ (- gax shift) (- alpha 1.0)))
                 (set! alpha (- alpha 1.0))
                 (loop))))
           gax))))
