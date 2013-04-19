#lang racket
;;; Racket Science Collection
;;; statistics.rkt
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
;;; This modules implements various statistical functions.  It is based on the
;;; Statistics in the GNU Scientific Library, which is licensed under the GPL.
;;;
;;; Version  Date      Description
;;; 1.0.0    09/24/04  Marked as ready for Release 1.0. (MDW)
;;; 2.0.0    11/19/07  Added unchecked versions of functions. (MDW)
;;; 3.0.0    06/07/08  More V4.0 changes.  (MDW)
;;; 3.0.1    07/01/08  Changed (when (not ...) ...) to (unless ... ...). (MDW)
;;; 3.1.0    09/12/09  Rewritten to allow statistics for any sequence; updated
;;;                    the license to LGPL V3; added correlation function; added
;;;                    running statistics. (MDW)
;;; 3.1.1    09/12/09  Added mean-and-variance that computes both in one pass.
;;;                    This is also used to avoid a third pass in computing skew
;;;                    and kurtosis. (MDW)
;;; 3.2.0    10/04/09  Added unsafe code. (MDW)
;;; 4.0.0    06/08/10  Changed the header and restructures the code. (MDW)
;;; 4.0.1    08/28/11  Fixed an 3 argument to call unsafe-fl* that Matthew found
;;;                    in weighted-sum-of-squares. (MDW)
;;; 4.0.2    12/21/11  Added skew and kurtosis to running statistics. Changed
;;;                    population/sample varianve, standard deviation, skew, and
;;;                    kurtosis to be more in line with Excel. (MDW)

(require scheme/unsafe/ops
         "unsafe-ops-utils.rkt")

;;; Running Statistics

;;; (struct statistics (n min max M1 M2 M3 M4)
;;;                            #:mutable
;;;                            #:transparent)
;;;   n : exact-nonegative-integer?
;;;   min : inexact-real?
;;;   max : inexact-real?
;;;   M1 : inexact-real?
;;;   M2 : inexact-real?
;;;   M3 : inexact-real?
;;;   M4 : inexact-real?
(struct statistics
  (n min max M1 M2 M3 M4)
  #:mutable
  #:transparent)

;;; (make-statistics) -> statistics?
;;; Returns a new, empty statistics instance.
(define (make-statistics)
  (statistics 0 +inf.0 -inf.0 0.0 0.0 0.0 0.0))

;;; (statistics-reset! rs) -> void?
;;;   rs : statistics?
;;; Resets the statistics instance rs.
(define (statistics-reset! rs)
  (set-statistics-n! rs 0)
  (set-statistics-min! rs +inf.0)
  (set-statistics-max! rs -inf.0)
  (set-statistics-M1! rs 0.0)
  (set-statistics-M2! rs 0.0)
  (set-statistics-M3! rs 0.0)
  (set-statistics-M4! rs 0.0))

;;; (statistics-tally! rs x) -> void?
;;;   rs : statistics?
;;;   x : real?
;;; Increments the statistics instance rs with sample x.
(define (statistics-tally! rs x)
  (with-float (x)
    (set-statistics-min! rs (min (statistics-min rs) x))
    (set-statistics-max! rs (max (statistics-max rs) x))
    (let* ((n (+ (statistics-n rs) 1))
           (delta (- x (statistics-M1 rs)))
           (delta-n (/ delta n))
           (delta-n2 (sqr delta-n))
           (term1 (* delta delta-n (statistics-n rs)))
           (M4 (+ (statistics-M4 rs)
                  (* term1 delta-n2 (+ (sqr n) (* -3 n) 3))
                  (* 6 delta-n2 (statistics-M2 rs))
                  (* -4 delta-n (statistics-M3 rs))))
           (M3 (+ (statistics-M3 rs)
                  (* term1 delta-n (- n 2))
                  (* -3 delta-n (statistics-M2 rs))))
           (M2 (+ (statistics-M2 rs) term1))
           (M1 (+ (statistics-M1 rs) delta-n)))
      (set-statistics-n! rs n)
      (set-statistics-M1! rs M1)
      (set-statistics-M2! rs M2)
      (set-statistics-M3! rs M3)
      (set-statistics-M4! rs M4))))

;;; (statistics-range rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the range of the running statistics instance rs.
(define (statistics-range rs)
  (- (statistics-max rs) (statistics-min rs)))

;;; (statistics-mean rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the mean of the statistics instance rs.
(define (statistics-mean rs)
  (statistics-M1 rs))

;;; (statistics-variance-p rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the population variance of the statistics instance rs.
(define (statistics-variance-p rs)
  (if (> (statistics-n rs) 0)
      (/ (statistics-M2 rs)
         (statistics-n rs))
      0.0))

;;; (statistics-standard-deviation-p rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the population standard deviation of the statistics instance rs.
(define (statistics-standard-deviation-p rs)
  (sqrt (statistics-variance-p rs)))

;;; (statistics-CV-p rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the population coefficient of variation of the statistics
;;; instance rs.
(define (statistics-CV-p rs)
  (if (not (= (statistics-mean rs) 0.0))
      (/ (statistics-standard-deviation-p rs)
         (statistics-mean rs))
      +nan.0))

;;; (statistics-variance rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the sample variance of the statistics instance rs.
(define (statistics-variance rs)
  (let ((n (statistics-n rs)))
    (if (> n 1)
        (* (/ n (- n 1)) (statistics-variance-p rs))
        +nan.0)))

;;; (statistics-standard-deviation rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the sample standard deviation of the statistics instance rs.
(define (statistics-standard-deviation rs)
  (sqrt (statistics-variance rs)))

;;; (statistics-CV rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the sample coefficient of variation of the statistics
;;; instance rs.
(define (statistics-CV rs)
  (if (not (= (statistics-mean rs) 0.0))
      (/ (statistics-standard-deviation rs)
         (statistics-mean rs))
      +nan.0))

;;; (statistics-skew-p rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the population skewness of the statistics instance rs.
(define (statistics-skew-p rs)
  (if (not (= (statistics-M2 rs) 0.0))
      (/ (* (sqrt (statistics-n rs)) (statistics-M3 rs))
         (expt (statistics-M2 rs) 3/2))
      +nan.0))

;;; (statistics-skew rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the sample skewness of the statistics instance rs.
(define (statistics-skew rs)
  (let ((n (statistics-n rs)))
    (if (> n 2)
        (* (/ (sqrt (* n (- n 1))) (- n 2))
           (statistics-skew-p rs))
        +nan.0)))

;;; (statistics-kurtosis-p rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the population kurtosis of the statistics instance rs.
(define (statistics-kurtosis-p rs)
  (if (not (= (statistics-M2 rs) 0.0))
      (- (/ (* (statistics-n rs) (statistics-M4 rs))
            (sqr (statistics-M2 rs)))
         3)
      +nan.0))

;;; (statistics-kurtosis-p rs) -> inexact-real?
;;;   rs : statistics?
;;; Returns the sample kurtosis of the statistics instance rs.
(define (statistics-kurtosis rs)
  (let ((n (statistics-n rs)))
    (if (> n 3)
        (* (/ (- n 1) (* (- n 2) (- n 3)))
           (+ (* (+ n 1) (statistics-kurtosis-p rs)) 6))
        +nan.0)))


;;; Dispatching for and for/fold
;;;
;;; The current implementation of for and for/fold can optimize certain
;;; sequences, for example, vectors and lists. These macros generate code that
;;; the JIT compiler can optimize for vectors and lists.

;;; (dispatch-for (for-clause ...)
;;;   body ...+)
(define-syntax dispatch-for
  (syntax-rules (rewrite-for-clauses)
    ((dispatch-for
      rewrite-for-clauses
      general-for-clauses
      ()
      vector-cond-expr-terms
      vector-for-clauses
      list-cond-expr-terms
      list-for-clauses
      expr ...)
     (cond ((and . vector-cond-expr-terms)
            (for vector-for-clauses
              expr ...))
           ((and . list-cond-expr-terms)
            (for list-for-clauses
              expr ...))
           (else
            (for general-for-clauses
              expr ...))))
    ((dispatch-for
      rewrite-for-clauses
      general-for-clauses
      ((id (sequence-type . sequence-args)) . rest-for-clauses)
      vector-cond-expr-terms
      vector-for-clauses
      list-cond-expr-terms
      list-for-clauses
      expr ...)
     (dispatch-for
      rewrite-for-clauses
      general-for-clauses
      rest-for-clauses
      vector-cond-expr-terms
      ((id (sequence-type . sequence-args)) . vector-for-clauses)
      list-cond-expr-terms
      ((id (sequence-type . sequence-args)) . list-for-clauses)
      expr ...))
    ((dispatch-for
      rewrite-for-clauses
      general-for-clauses
      ((id data) . rest-for-clauses)
      vector-cond-expr-terms
      vector-for-clauses
      list-cond-expr-terms
      list-for-clauses
      expr ...)
     (dispatch-for
      rewrite-for-clauses
      general-for-clauses
      rest-for-clauses
      ((vector? data) . vector-cond-expr-terms)
      ((id (in-vector data)) . vector-for-clauses)
      ((pair? data) . list-cond-expr-terms)
      ((id (in-list data)) . list-for-clauses)
      expr ...))
    ((dispatch-for
      general-for-clauses
      expr ...)
     (dispatch-for
      rewrite-for-clauses
      general-for-clauses
      general-for-clauses
      ()
      ()
      ()
      ()
      expr ...))))

;;; (dispatch-for/fold (accumulator-clause ...)
;;;                    (for-clause ...)
;;;   body ...+)
(define-syntax dispatch-for/fold
  (syntax-rules (rewrite-for-clauses)
    ((dispatch-for/fold
      rewrite-for-clauses
      accumulator-clauses
      general-for-clauses
      ()
      vector-cond-expr-terms
      vector-for-clauses
      list-cond-expr-terms
      list-for-clauses
      expr ...)
     (cond ((and . vector-cond-expr-terms)
            (for/fold accumulator-clauses
                      vector-for-clauses
              expr ...))
           ((and . list-cond-expr-terms)
            (for/fold accumulator-clauses
                      list-for-clauses
              expr ...))
           (else
            (for/fold accumulator-clauses
                      general-for-clauses
              expr ...))))
    ((dispatch-for/fold
      rewrite-for-clauses
      accumulator-clauses
      general-for-clauses
      ((id (sequence-type . sequence-args)) . rest-for-clauses)
      vector-cond-expr-terms
      vector-for-clauses
      list-cond-expr-terms
      list-for-clauses
      expr ...)
     (dispatch-for/fold
      rewrite-for-clauses
      accumulator-clauses
      general-for-clauses
      rest-for-clauses
      vector-cond-expr-terms
      ((id (sequence-type . sequence-args)) . vector-for-clauses)
      list-cond-expr-terms
      ((id (sequence-type . sequence-args)) . list-for-clauses)
      expr ...))
    ((dispatch-for/fold
      rewrite-for-clauses
      accumulator-clauses
      general-for-clauses
      ((id data) . rest-for-clauses)
      vector-cond-expr-terms
      vector-for-clauses
      list-cond-expr-terms
      list-for-clauses
      expr ...)
     (dispatch-for/fold
      rewrite-for-clauses
      accumulator-clauses
      general-for-clauses
      rest-for-clauses
      ((vector? data) . vector-cond-expr-terms)
      ((id (in-vector data)) . vector-for-clauses)
      ((pair? data) . list-cond-expr-terms)
      ((id (in-list data)) . list-for-clauses)
      expr ...))
    ((dispatch-for/fold
      accumulator-clauses
      general-for-clauses
      expr ...)
     (dispatch-for/fold
      rewrite-for-clauses
      accumulator-clauses
      general-for-clauses
      general-for-clauses
      ()
      ()
      ()
      ()
      expr ...))))
      
;;; Mean and Standard Deviation and Variance

;;; (mean data) -> inexact-real?
;;;   data : sequence-of-real?
;;; Returns the arithmetic mean of data.
(define (mean data)
  (dispatch-for/fold ((m-old 0.0))
                     ((i (in-naturals 1))
                      (x data))
    (with-float (x)
      (unsafe-fl+ m-old
                  (unsafe-fl/ (unsafe-fl- x m-old)
                              (unsafe-fx->fl i))))))

;;; (mean-and-variance-p data) -> inexact-real? (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
(define (mean-and-variance-p data)
  (let-values 
      (((n m s)
        (dispatch-for/fold ((i-old 0)
                            (m-old 0.0)
                            (s-old 0.0))
                           ((x data))
          (with-float (x)
            (let ((i-new (unsafe-fx+ i-old 1)))
              (if (unsafe-fx= i-new 1)
                  (values i-new x 0.0)
                  (let* ((m-new (unsafe-fl+ m-old
                                            (unsafe-fl/ (unsafe-fl- x m-old)
                                                        (exact->inexact i-new))))
                         (s-new (unsafe-fl+ s-old
                                            (unsafe-fl* (unsafe-fl- x m-old)
                                                        (unsafe-fl- x m-new)))))
                    (values i-new m-new s-new))))))))
    (values m (if (unsafe-fx> n 1)
                  (unsafe-fl/ s (exact->inexact n))
                  0.0))))

;;; (mean-and-variance data) -> inexact-real? (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
(define (mean-and-variance data)
  (let-values 
      (((n m s)
        (dispatch-for/fold ((i-old 0)
                            (m-old 0.0)
                            (s-old 0.0))
                           ((x data))
          (with-float (x)
            (let ((i-new (unsafe-fx+ i-old 1)))
              (if (unsafe-fx= i-new 1)
                  (values i-new x 0.0)
                  (let* ((m-new (unsafe-fl+ m-old
                                            (unsafe-fl/ (unsafe-fl- x m-old)
                                                        (exact->inexact i-new))))
                         (s-new (unsafe-fl+ s-old
                                            (unsafe-fl* (unsafe-fl- x m-old)
                                                        (unsafe-fl- x m-new)))))
                    (values i-new m-new s-new))))))))
    (values m (if (unsafe-fx> n 1)
                  (unsafe-fl/ s (exact->inexact (unsafe-fx- n 1)))
                  0.0))))

;;; (compute-variance data mean) -> exact-nonnegative-integer?
;;;                                 (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
;;;   mean : inexact-real?
(define (compute-variance data mean)
  (dispatch-for/fold ((i 0)
                      (v-old 0.0))
                     ((x data))
    (with-float (x)
      (let* ((n (unsafe-fx+ i 1))
             (delta (unsafe-fl- x mean))
             (v-new (unsafe-fl+
                     v-old
                     (unsafe-fl/ (unsafe-fl- (unsafe-fl* delta delta) v-old)
                                 (exact->inexact (unsafe-fx+ i 1))))))
        (values n v-new)))))

;;; (variance-with-fixed-mean data mean) -> (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
;;;   mean : real?
(define (variance-with-fixed-mean data mean)
  (with-float (mean)
    (let-values
        (((n V)
          (compute-variance data mean)))
      V)))

;;; (standard-deviation-with-fixed-mean data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real?
(define (standard-deviation-with-fixed-mean data mean)
  (sqrt (variance-with-fixed-mean data mean)))

;;; (variance-p data mean) -> (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define variance-p
  (case-lambda
    ((data mean)
     (variance-with-fixed-mean data mean))
    ((data)
     (variance-p data (mean data)))))

;;; (standard-deviation-p data mean) -> (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define standard-deviation-p
  (case-lambda
    ((data mean)
     (sqrt (variance-p data mean)))
    ((data)
     (standard-deviation-p data (mean data)))))

;;; (variance data mean) -> (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define variance
  (case-lambda
    ((data mean)
     (with-float (mean)
       (let-values
           (((n V)
             (compute-variance data mean)))
         (if (> n 1)
             (unsafe-fl* V (unsafe-fl/ (exact->inexact n)
                                       (exact->inexact (unsafe-fx- n 1))))
             0.0))))
    ((data)
     (variance data (mean data)))))

;;; (standard-deviation data mean) -> (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define standard-deviation
  (case-lambda
    ((data mean)
     (sqrt (variance data mean)))
    ((data)
     (standard-deviation data (mean data)))))

;;; (sum-of-squares data mean) -> (>=/c 0.0)
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
(define (sum-of-squares data (mean (mean data)))
  (with-float (mean)
    (dispatch-for/fold ((tss-old 0.0))
                       ((x data))
      (with-float (x)
        (let ((delta (unsafe-fl- x mean)))
          (unsafe-fl+ tss-old (unsafe-fl* delta delta)))))))

;;; Absolute Deviation

;;; (absolute-deviation data mean) -> (and/c inexact-real? (>=/c 0.0))
;;;   data : sequence-of-real?
;;;   mean : real? (mean data)
(define (absolute-deviation data (mean (mean data)))
  (with-float (mean)
    (let-values
        (((n S)
          (dispatch-for/fold ((i 0)
                              (sum 0.0))
                             ((x data))
            (with-float (x)
              (let ((delta (abs (unsafe-fl- x mean))))
                (values (unsafe-fx+ i 1)
                        (unsafe-fl+ sum delta)))))))
      (unsafe-fl/ S (exact->inexact n)))))

;;; Higher Moments (Skewness and Kurtosis)

(define (compute-skew data mean sd)
  (with-float (mean sd)
    (dispatch-for/fold ((i 0)
                        (skew 0.0))
                       ((x data))
      (with-float (x)
        (let ((delta (unsafe-fl/ (unsafe-fl- x mean) sd)))
          (values (unsafe-fx+ i 1)
                  (unsafe-fl+ skew
                              (unsafe-fl/
                               (unsafe-fl-
                                (unsafe-fl* delta
                                            (unsafe-fl* delta delta))
                                skew)
                               (exact->inexact (unsafe-fx+ i 1))))))))))

;;; (skew-p data mean sd) -> inexact-real?
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
;;;   sd : (>=/c 0.0) = (standard-deviation data)
;;; (skew-p data) -> inexact-real?
;;;   data : sequence-of-real?
(define skew-p
  (case-lambda
    ((data mean sd)
     (let-values (((n S3) (compute-skew data mean sd)))
       S3))
    ((data)
     (let* ((mu (mean data))
            (sigma (standard-deviation-with-fixed-mean data mu)))
       (skew-p data mu sigma)))))

;;; (skew data mean sd) -> inexact-real?
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
;;;   sd : (>=/c 0.0) = (standard-deviation data)
;;; (skew data) -> inexact-real?
;;;   data : sequence-of-real?
(define skew
  (case-lambda
    ((data mean sd)
     (let-values (((n S3) (compute-skew data mean sd)))
       (if (> n 2)
           (* (/ (sqrt (* n (- n 1))) (- n 2)) S3)
           +nan.0)))
    ((data)
     (let* ((mu (mean data))
            (sigma (standard-deviation-with-fixed-mean data mu)))
       (skew data mu sigma)))))

(define (compute-kurtosis data mean sd)
  (with-float (mean sd)
    (dispatch-for/fold ((i 0)
                        (avg 0.0))
                       ((x data))
      (with-float (x)
        (let ((delta (unsafe-fl/ (unsafe-fl- x mean) sd)))
          (values (unsafe-fx+ i 1)
                  (unsafe-fl+ avg
                              (unsafe-fl/
                               (unsafe-fl-
                                (unsafe-fl* (unsafe-fl* delta delta)
                                            (unsafe-fl* delta delta))
                                avg)
                               (exact->inexact (unsafe-fx+ i 1))))))))))

;;; (kurtosis-p data mean sd) -> inexact-real?
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
;;;   sd : (>=/c 0.0) = (standard-deviation data)
;;; (kurtosis-p data) -> inexact-real?
;;;   data : sequence-of-real?
(define kurtosis-p
  (case-lambda
    ((data mean sd)
     (let-values (((n S4) (compute-kurtosis data mean sd)))
       (- S4 3.0)))
    ((data)
     (let* ((mu (mean data))
            (sigma (standard-deviation-with-fixed-mean data mu)))
       (kurtosis-p data mu sigma)))))

;;; (kurtosis data mean sd) -> inexact-real?
;;;   data : sequence-of-real?
;;;   mean : real? = (mean data)
;;;   sd : (>=/c 0.0) = (standard-deviation data)
;;; (kurtosis data) -> inexact-real?
;;;   data : sequence-of-real?
(define kurtosis
  (case-lambda
    ((data mean sd)
     (let-values (((n S4) (compute-kurtosis data mean sd)))
       (if (> n 3)
           (* (/ (- n 1) (* (- n 2) (- n 3)))
              (+ (* (+ n 1) (- S4 3.0)) 6.0))
           +nan.0)))
    ((data)
     (let* ((mu (mean data))
            (sigma (standard-deviation-with-fixed-mean data mu)))
       (kurtosis data mu sigma)))))

;;; Autocorrelation

;;; (lag-1-autocorrelation data mean) -> inexact-real?
;;;   data : nonempty-sequence-of-real?
;;;   mean : real? = (mean data)
(define (lag-1-autocorrelation data (mean (mean data)))
  (with-float (mean)
    (let-values
        (((x-prev Q V)
          (dispatch-for/fold ((x-prev 0.0)
                              (q-old 0.0)
                              (v-old 0.0))
                             ((i (in-naturals))
                              (x data))
            (with-float (x)
              (if (unsafe-fx= i 0)
                  (let ((delta (unsafe-fl- x mean)))
                    (values x 0.0 (unsafe-fl* delta delta)))
                  (let* ((delta0 (unsafe-fl- x-prev mean))
                         (delta1 (unsafe-fl- x mean))
                         (q-new (unsafe-fl+
                                 q-old
                                 (unsafe-fl/ (unsafe-fl- (unsafe-fl* delta0 delta1)
                                                         q-old)
                                             (exact->inexact (unsafe-fx+ i 1)))))
                         (v-new (unsafe-fl+
                                 v-old
                                 (unsafe-fl/ (unsafe-fl- (unsafe-fl* delta1 delta1)
                                                         v-old)
                                             (exact->inexact (unsafe-fx+ i 1))))))
                    (values x q-new v-new)))))))
      (unsafe-fl/ Q V))))

;;; Covariance

;;; (covariance data1 data2 mean1 mean2) -> inexact-real?
;;;   data1 : sequence-of-real?
;;;   data2 : sequence-of-real?
;;;   mean1 : real? = (mean data1)
;;;   mean2 : real? = (mean data2)
(define (covariance data1 data2 (mean1 (mean data1)) (mean2 (mean data2)))
  (with-float (mean1 mean2)
    (let-values
        (((n covariance)
          (dispatch-for/fold ((i 0)
                              (covariance 0.0))
                             ((x1 data1)
                              (x2 data2))
            (with-float (x1 x2)
              (let* ((delta1 (unsafe-fl- x1 mean1))
                     (delta2 (unsafe-fl- x2 mean2))
                     (covariance-new
                      (unsafe-fl+ covariance
                                  (unsafe-fl/ (unsafe-fl- (unsafe-fl* delta1 delta2)
                                                          covariance)
                                              (exact->inexact (unsafe-fx+ i 1))))))
                (values (unsafe-fx+ i 1) covariance-new))))))
      (unsafe-fl* covariance (unsafe-fl/ (exact->inexact n)
                                         (exact->inexact (unsafe-fx- n 1)))))))

;;; Correlation

;;; (correlation data1 data2) -> (and/c inexact-real? (real-in -1.0 1.0))
;;;  data1 : nonempty-sequence-of-real?
;;;  data2 : nonempty-sequence-of-real?
(define (correlation data1 data2)
  (let-values
      (((sum-xsq sum-ysq sum-cross mean-x mean-y)
        (dispatch-for/fold ((sum-xsq 0.0)
                            (sum-ysq 0.0)
                            (sum-cross 0.0)
                            (mean-x 0.0)
                            (mean-y 0.0))
                           ((i (in-naturals))
                            (x data1)
                            (y data2))
          (with-float (x y)
            (if (unsafe-fx= i 0)
                (values 0.0 0.0 0.0 x y)
                (let* ((ratio (unsafe-fl/ (exact->inexact i)
                                          (exact->inexact (unsafe-fx+ i 1))))
                       (delta-x (unsafe-fl- x mean-x))
                       (delta-y (unsafe-fl- y mean-y)))
                  (values (unsafe-fl+ sum-xsq
                                      (unsafe-fl* (unsafe-fl* delta-x delta-x)
                                                  ratio))
                          (unsafe-fl+ sum-ysq
                                      (unsafe-fl* (unsafe-fl* delta-y delta-y)
                                                  ratio))
                          (unsafe-fl+ sum-cross
                                      (unsafe-fl* (unsafe-fl* delta-x delta-y)
                                                  ratio))
                          (unsafe-fl+ mean-x
                                      (unsafe-fl/ delta-x
                                                  (exact->inexact (unsafe-fx+ i 1))))
                          (unsafe-fl+ mean-y
                                      (unsafe-fl/ delta-y
                                                  (exact->inexact (unsafe-fx+ i 1)))))))))))
    (unsafe-fl/ sum-cross (unsafe-fl* (sqrt sum-xsq) (sqrt sum-ysq)))))

;;; Weighted Samples

;;; (weighted-mean weights data) -> inexact-real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
(define (weighted-mean weights data)
  (let-values
      (((W M)
        (dispatch-for/fold ((w-old 0.0)
                            (m-old 0.0))
                          ((w weights)
                           (x data))
          (with-float (w x)
            (if (unsafe-fl> w 0.0)
                (let* ((w-new (unsafe-fl+ w-old w))
                       (m-new (unsafe-fl+
                               m-old
                               (unsafe-fl* (unsafe-fl- x m-old)
                                           (unsafe-fl/ w w-new)))))
                  (values w-new m-new))
                (values w-old m-old))))))
    M))

;;; (compute-weighted-variance weights data wmean) -> exact-nonnegative-integer>
;;;                                                   inexact-real?
;;;                                                   inexact-real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : inexact-real?
(define (compute-weighted-variance weights data wmean)
  (with-float (wmean)
    (dispatch-for/fold ((i 0)
                        (w-old 0.0)
                        (wv-old 0.0))
                       ((w weights)
                        (x data))
      (with-float (w x)
        (if (unsafe-fl> w 0.0)
            (let* ((delta (unsafe-fl- x wmean))
                   (w-new (unsafe-fl+ w-old w))
                   (wv-new (unsafe-fl+
                            wv-old
                            (unsafe-fl* (unsafe-fl- (unsafe-fl* delta delta)
                                                    wv-old)
                                        (unsafe-fl/ w w-new)))))
              (values (unsafe-fx+ i 1) w-new wv-new))
            (values (unsafe-fx+ i 1) w-old wv-old))))))

;;; (compute-factor weights) -> inexact-real?
;;;   weights : sequence-of-real?
(define (compute-factor weights)
  (let-values
      (((a b)
        (dispatch-for/fold ((a-old 0.0)
                            (b-old 0.0))
                           ((w weights))
          (with-float (w)
            (if (unsafe-fl> w 0.0)
                (values (unsafe-fl+ a-old w) (unsafe-fl+ b-old (unsafe-fl* w w)))
                (values a-old b-old))))))
    (unsafe-fl/ (unsafe-fl* a a) (unsafe-fl- (unsafe-fl* a a) b))))

;;; (weighted-variance-with-fixed-mean weights data wmean)
;;; -> (and/c inexact-real? (>=/c 0.0))
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real?
(define (weighted-variance-with-fixed-mean weights data wmean)
  (with-float (wmean)
    (let-values
        (((n W wvariance)
          (compute-weighted-variance weights data wmean)))
      wvariance)))

;;; (weighted-standard-deviation-with-fixed-mean weights data wmean)
;;; -> (and/c inexact-real? (>=/c 0.0))
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real?
(define (weighted-standard-deviation-with-fixed-mean weights data wmean)
  (with-float (wmean)
    (let-values
        (((n W wvariance)
          (compute-weighted-variance weights data wmean)))
      (sqrt wvariance))))

;;; (weighted-variance weights data wmean) -> (and/c inexact-real? (>=/c 0.0))
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-variance weights data (wmean (weighted-mean weights data)))
  (with-float (wmean)
    (let-values
        (((n W wvariance)
          (compute-weighted-variance weights data wmean)))
      (let ((scale (compute-factor weights)))
        (unsafe-fl* scale wvariance)))))

;;; (weighted-standard-deviation weights data wmean) -> (and/c inexact-real? (>=/c 0.0))
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-standard-deviation weights data (wmean (weighted-mean weights data)))
  (with-float (wmean)
    (let-values
        (((n W wvariance)
          (compute-weighted-variance weights data wmean)))
      (let ((scale (compute-factor weights)))
        (sqrt (unsafe-fl* scale wvariance))))))

;;; (weighted-sum-of-squares weights data wmean) -> (and/c inexact-real? (>=/c 0.0))
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-sum-of-squares weights data (wmean (weighted-mean weights data)))
  (with-float (wmean)
    (dispatch-for/fold ((wtss-old 0.0))
                       ((w weights)
                        (x data))
      (with-float (w x)
        (if (unsafe-fl> w 0.0)
            (let ((delta (unsafe-fl- x wmean)))
              (unsafe-fl+ wtss-old (unsafe-fl* w (unsafe-fl* delta delta))))
            wtss-old)))))

;;; (weighted-absolute-deviation weights data wmean) -> (and/c inexact-real? (>=/c 0.0))
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
(define (weighted-absolute-deviation weights data (wmean (weighted-mean weights data)))
  (with-float (wmean)
    (let-values
        (((W wabsdev)
          (dispatch-for/fold ((w-old 0.0)
                              (wabsdev-old 0.0))
                             ((w weights)
                              (x data))
            (with-float (w x)
              (if (unsafe-fl> w 0.0)
                  (let* ((delta (abs (unsafe-fl- x wmean)))
                         (w-new (unsafe-fl+ w-old w))
                         (wabsdev-new (unsafe-fl+
                                       wabsdev-old
                                       (unsafe-fl* (unsafe-fl- delta wabsdev-old)
                                                   (unsafe-fl/ w w-new)))))
                    (values w-new wabsdev-new))
                  (values w-old wabsdev-old))))))
      wabsdev)))

;;; (weighted-skew weights data wmean wsd) -> inexact-real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
;;;   wsd : (>=/c 0.0) = (weighted-standard-deviation weights data)
(define weighted-skew
  (case-lambda
    ((weights data wmean wsd)
     (with-float (wmean wsd)
       (let-values
           (((W wskew)
             (dispatch-for/fold ((w-old 0.0)
                                 (wskew-old 0.0))
                                ((w weights)
                                 (x data))
               (with-float (w x)
                 (if (unsafe-fl> w 0.0)
                     (let* ((delta (unsafe-fl/ (unsafe-fl- x wmean) wsd))
                            (w-new (unsafe-fl+ w-old w))
                            (wskew-new (unsafe-fl+
                                        wskew-old
                                        (unsafe-fl*
                                         (unsafe-fl- (unsafe-fl* delta
                                                                 (unsafe-fl* delta delta))
                                                     wskew-old)
                                         (unsafe-fl/ w w-new)))))
                       (values w-new wskew-new))
                     (values w-old wskew-old))))))
         wskew)))
    ((weights data)
     (let* ((wmean (weighted-mean weights data))
            (wsd (weighted-standard-deviation weights data wmean)))
       (weighted-skew weights data wmean wsd)))))

;;; (weighted-kurtosis weights data wmean wsd) -> inexact-real?
;;;   weights : sequence-of-real?
;;;   data : sequence-of-real?
;;;   wmean : real? = (weighted-mean weights data)
;;;   wsd : (>=/c 0.0) = (weighted-standard-deviation weights data)
(define weighted-kurtosis
  (case-lambda
    ((weights data wmean wsd)
     (with-float (wmean wsd)
       (let-values
           (((W wavg)
             (dispatch-for/fold ((w-old 0.0)
                                 (wavg-old 0.0))
                                ((w weights)
                                 (x data))
               (with-float (w x)
                 (if (unsafe-fl> w 0.0)
                     (let* ((delta (unsafe-fl/ (unsafe-fl- x wmean) wsd))
                            (w-new (unsafe-fl+ w-old w))
                            (wavg-new (unsafe-fl+
                                       wavg-old
                                       (unsafe-fl* (unsafe-fl-
                                                    (unsafe-fl* (unsafe-fl* delta delta)
                                                                (unsafe-fl* delta delta))
                                                    wavg-old)
                                                   (unsafe-fl/ w w-new)))))
                       (values w-new wavg-new))
                     (values w-old wavg-old))))))
         (unsafe-fl- wavg 3.0))))
    ((weights data)
     (let* ((wmean (weighted-mean weights data))
            (wsd (weighted-standard-deviation weights data wmean)))
       (weighted-kurtosis weights data wmean wsd)))))

;;; Maximum and Minimum Values

;;; (minimum-maximum-and-indices data) -> real?
;;;                                       real?
;;;                                       exact-nonnegative-integer?
;;;                                       exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (minimum-maximum-and-indices data)
  (dispatch-for/fold ((min +inf.0)
                      (max -inf.0)
                      (min-ndx -1)
                      (max-ndx -1))
                     ((i (in-naturals))
                      (x data))
    (let ((min-new (if (< x min) x min))
          (max-new (if (> x max) x max))
          (min-ndx-new (if (< x min) i min-ndx))
          (max-ndx-new (if (> x max) i max-ndx)))
      (values min-new max-new min-ndx-new max-ndx-new))))

;;; (minimum-maximum data) -> real?
;;;                           real?
;;;   data : nonempty-sequence-of-real?
(define (minimum-maximum data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    (values min max)))

;;; (minimum data) -> real?
;;;   data : nonempty-sequence-of-real?
(define (minimum data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    min))

;;; (maximum data) -> real?
;;;   data : nonempty-sequence-of-real?
(define (maximum data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    max))

;;; (minimum-maximum-index data) -> exact-nonnegative-integer?
;;;                                 exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (minimum-maximum-index data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    (values min-ndx max-ndx)))

;;; (minimum-index data) -> exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (minimum-index data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    min-ndx))

;;; (maximum-index data) -> exact-nonnegative-integer?
;;;   data : nonempty-sequence-of-real?
(define (maximum-index data)
  (let-values (((min max min-ndx max-ndx)
                (minimum-maximum-and-indices data)))
    max-ndx))

;;; Median and Quantiles

;;; (median-from-sorted-data sorted-data n) -> real?
;;;   sorted-data : nonempty-sorted-vector-of-real?
;;;   n : exact-nonnegative-integer? = (vector-length sorted-data)
(define (median-from-sorted-data sorted-data (n (vector-length sorted-data)))
  (when (> n (vector-length sorted-data))
    (error 'median-from-sorted-data
           "expected integer less than ~a, given ~a"
           (vector-length sorted-data) n))
  (let* ((lhs (quotient (- n 1) 2))
         (rhs (quotient n 2)))
    (if (= lhs rhs)
        (vector-ref sorted-data lhs)
        (/ (+ (vector-ref sorted-data lhs)
              (vector-ref sorted-data rhs))
           2.0))))

;;; (quantile-from-sorted-data sorted-data f n) -> real?
;;;   sorted-data : nonempty-sorted-vector-of-real?
;;;   n : exact-nonnegative-integer? = (vector-length sorted-data)
(define (quantile-from-sorted-data sorted-data f (n (vector-length sorted-data)))
  (when (> n (vector-length sorted-data))
    (error 'median-from-sorted-data
           "expected integer less than ~a, given ~a"
           (vector-length sorted-data) n))
  (let* ((index (* f (- n 1)))
         (lhs (inexact->exact (truncate index)))
         (delta (- index lhs)))
    (if (= lhs (- n 1))
        (vector-ref sorted-data lhs)
        (+ (* (- 1.0 delta) (vector-ref sorted-data lhs))
           (* delta (vector-ref sorted-data (+ lhs 1)))))))

;;; Module Contracts

(provide
 (rename-out (mean unchecked-mean)
             (mean-and-variance unchecked-mean-and-variance)
             (variance-with-fixed-mean unchecked-variance-with-fixed-mean)
             (standard-deviation-with-fixed-mean
              unchecked-standard-deviation-with-fixed-mean)
             (variance unchecked-variance)
             (standard-deviation unchecked-standard-deviation)
             (sum-of-squares unchecked-sum-of-squares)
             (absolute-deviation unchecked-absolute-deviation)
             (skew unchecked-skew)
             (kurtosis unchecked-kurtosis)
             (lag-1-autocorrelation unchecked-lag-1-autocorrelation)
             (covariance unchecked-covariance)
             (correlation unchecked-correlation)
             (weighted-mean unchecked-weighted-mean)
             (weighted-variance-with-fixed-mean
              unchecked-weighted-variance-with-fixed-mean)
             (weighted-standard-deviation-with-fixed-mean
              unchecked-weighted-standard-deviation-with-fixed-mean)
             (weighted-variance unchecked-weighted-variance)
             (weighted-standard-deviation unchecked-weighted-standard-deviation)
             (weighted-sum-of-squares unchecked-weighted-sum-of-squares)
             (weighted-absolute-deviation unchecked-weighted-absolute-deviation)
             (weighted-skew unchecked-weighted-skew)
             (weighted-kurtosis unchecked-weighted-kurtosis)
             (minimum-maximum unchecked-minimum-maximum)
             (minimum unchecked-minimum)
             (maximum unchecked-maximum)
             (minimum-maximum-index unchecked-minimum-maximum-index)
             (minimum-index unchecked-minimum-index)
             (maximum-index unchecked-maximum-index)
             (median-from-sorted-data unchecked-median-from-sorted-data)
             (quantile-from-sorted-data unchecked-quantile-from-sorted-data)))

;;; Contracts

;;; sequence-of-real? : contract?
(define sequence-of-real?
  (flat-named-contract
   "sequence-of-real?"
   (lambda (s)
     (and (sequence? s)
          (let/ec exit
            (dispatch-for ((x s))
              (unless (real? x)
                (exit #f)))
            #t)))))

;;; nonempty-sequence-of-real? : contract?
(define nonempty-sequence-of-real?
  (flat-named-contract
   "nonempty-sequence-of-real?"
   (lambda (s)
     (and (sequence? s)
          (let ((empty? #t))
            (let/ec exit
              (dispatch-for ((x s))
                (set! empty? #f)
                (unless (real? x)
                  (exit #f)))
              (not empty?)))))))

;;; nonempty-sorted-vector-of-real? : contract?
(define nonempty-sorted-vector-of-real?
  (flat-named-contract
   "nonempty-sorted-vector-of-real?"
   (lambda (v)
     (and (vector v)
          (let ((empty? #t))
            (let/ec exit
              (for/fold ((x-old -inf.0))
                        ((x (in-vector v)))
                (set! empty? #f)
                (when (or (not (real? x))
                          (< x x-old))
                  (exit #f))
                x)
              (not empty?)))))))

(define nan/c
  (flat-named-contract
   "non-a-number"
   (lambda (v)
     (eqv? v +nan.0))))

(provide/contract
 ;;; Running Statistics
 (statistics?
  (-> any/c boolean?))
 (make-statistics
  (-> statistics?))
 (statistics-reset!
  (-> statistics? void?))
 (statistics-tally!
  (-> statistics? real? void?))
 (statistics-n
  (-> statistics? exact-nonnegative-integer?))
 (statistics-min
  (-> statistics? inexact-real?))
 (statistics-max
  (-> statistics? inexact-real?))
 (statistics-range
  (-> statistics? inexact-real?))
 (statistics-mean
  (-> statistics? inexact-real?))
 (statistics-variance-p
  (-> statistics? (and/c inexact-real? (or/c nan/c (>=/c 0.0)))))
 (statistics-standard-deviation-p
  (-> statistics? (and/c inexact-real? (or/c nan/c (>=/c 0.0)))))
 (statistics-CV-p
  (-> statistics? inexact-real?))
 (statistics-variance
  (-> statistics? (and/c inexact-real? (or/c nan/c (>=/c 0.0)))))
 (statistics-standard-deviation
  (-> statistics? (and/c inexact-real? (or/c nan/c (>=/c 0.0)))))
 (statistics-CV
  (-> statistics? inexact-real?))
 (statistics-skew-p
  (-> statistics? inexact-real?))
 (statistics-skew
  (-> statistics? inexact-real?))
 (statistics-kurtosis-p
  (-> statistics? inexact-real?))
 (statistics-kurtosis
  (-> statistics? inexact-real?))
 ;;; Mean and Standard Deviation and Variance
 (mean
  (-> sequence-of-real? inexact-real?))
 (mean-and-variance-p
  (-> sequence-of-real?
      (values inexact-real? (and/c inexact-real? (>=/c 0.0)))))
 (mean-and-variance
  (-> sequence-of-real?
      (values inexact-real? (and/c inexact-real? (>=/c 0.0)))))
 (variance-with-fixed-mean
  (-> sequence-of-real? real? (and/c inexact-real? (>=/c 0.0))))
 (standard-deviation-with-fixed-mean
  (-> sequence-of-real? real? (and/c inexact-real? (>=/c 0.0))))
 (variance-p
  (case-> (-> sequence-of-real? real? (and/c inexact-real? (>=/c 0.0)))
          (-> sequence-of-real? (and/c inexact-real? (>=/c 0.0)))))
 (standard-deviation-p
  (->* (sequence-of-real?) (real?) (and/c inexact-real? (>=/c 0.0))))
 (variance
  (case-> (-> sequence-of-real? real? (and/c inexact-real? (>=/c 0.0)))
          (-> sequence-of-real? (and/c inexact-real? (>=/c 0.0)))))
 (standard-deviation
  (->* (sequence-of-real?) (real?) (and/c inexact-real? (>=/c 0.0))))
 (sum-of-squares
  (->* (sequence-of-real?) (real?) (and/c inexact-real? (>=/c 0.0))))
 ;; Absolute Deviation
 (absolute-deviation
  (->* (sequence-of-real?) (real?) (and/c inexact-real? (>=/c 0.0))))
 ;; Higher Moments (Skewness and Kurtosis)
 (skew-p
  (case-> (-> sequence-of-real? real? (>=/c 0.0) inexact-real?)
          (-> sequence-of-real? inexact-real?)))
 (skew
  (case-> (-> sequence-of-real? real? (>=/c 0.0) inexact-real?)
          (-> sequence-of-real? inexact-real?)))
 (kurtosis-p
  (case-> (-> sequence-of-real? real? (>=/c 0.0) inexact-real?)
          (-> sequence-of-real? inexact-real?)))
 (kurtosis
  (case-> (-> sequence-of-real? real? (>=/c 0.0) inexact-real?)
          (-> sequence-of-real? inexact-real?)))
 ;; Autocorrelation
 (lag-1-autocorrelation
  (->* (nonempty-sequence-of-real?) (real?) inexact-real?))
 ;; Covariance
 (covariance
  (->* (sequence-of-real? sequence-of-real?)
       (real? real?)
       inexact-real?))
 ;; Correlation
 (correlation
  (-> nonempty-sequence-of-real? nonempty-sequence-of-real?
      (and/c inexact-real? (real-in -1.0 1.0))))
 ;; Weighted Samples
 (weighted-mean
  (-> sequence-of-real? sequence-of-real? inexact-real?))
 (weighted-variance-with-fixed-mean
  (-> sequence-of-real? sequence-of-real? real?
      (and/c inexact-real? (>=/c 0.0))))
 (weighted-standard-deviation-with-fixed-mean
  (-> sequence-of-real? sequence-of-real? real?
      (and/c inexact-real? (>=/c 0.0))))
 (weighted-variance
  (->* (sequence-of-real? sequence-of-real?) (real?)
       (and/c inexact-real? (>=/c 0.0))))
 (weighted-standard-deviation
  (->* (sequence-of-real? sequence-of-real?) (real?)
       (and/c inexact-real? (>=/c 0.0))))
 (weighted-sum-of-squares
  (->* (sequence-of-real? sequence-of-real?) (real?)
       (and/c inexact-real? (>=/c 0.0))))
 (weighted-absolute-deviation
  (->* (sequence-of-real? sequence-of-real?) (real?)
       (and/c inexact-real? (>=/c 0.0))))
 (weighted-skew
  (case-> (-> sequence-of-real? sequence-of-real? real? (>=/c 0.0) inexact-real?)
          (-> sequence-of-real? sequence-of-real? inexact-real?)))
 (weighted-kurtosis
  (case-> (-> sequence-of-real? sequence-of-real? real? (>=/c 0.0) inexact-real?)
          (-> sequence-of-real? sequence-of-real? inexact-real?)))
 ;; Maximum and Minimum Values
 (minimum-maximum
  (-> nonempty-sequence-of-real? (values real? real?)))
 (minimum
  (-> nonempty-sequence-of-real? real?))
 (maximum
  (-> nonempty-sequence-of-real? real?))
 (minimum-maximum-index
  (-> nonempty-sequence-of-real? (values exact-nonnegative-integer?
                                         exact-nonnegative-integer?)))
 (minimum-index
  (-> nonempty-sequence-of-real? exact-nonnegative-integer?))
 (maximum-index
  (-> nonempty-sequence-of-real? exact-nonnegative-integer?))
 ;; Median and Quantiles
 (median-from-sorted-data
  (->* (nonempty-sorted-vector-of-real?) (exact-nonnegative-integer?) real?))
 (quantile-from-sorted-data
  (->* (nonempty-sorted-vector-of-real? (real-in 0 1))
  (exact-nonnegative-integer?) real?)))