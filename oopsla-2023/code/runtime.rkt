#lang racket/base

(provide
  benchmark->perf#
  benchmark->raw#
  ;; stddev
  benchmark->perf#/stddev
  benchmark->perf#/2stddev
  benchmark->perf#/variance
  benchmark->perf#/t99
  benchmark->perf#/t95
  benchmark->perf#/t90
  benchmark->perf#/t80
  t-test
  shortnum
  )

(require
  (only-in racket/file file->value)
  (only-in math/statistics mean variance/mean stddev/mean)
  (only-in racket/math exact-round)
  racket/runtime-path)

(define-runtime-path runtime-data-dir "../runtime/")

(define ww 100)

(define (shortnum n)
  (/ (exact-round (* n ww)) ww))

(define (benchmark->perf#/stddev bm)
  (benchmark->perf#/X bm stddev/mean))

(define (benchmark->perf#/2stddev bm)
  (benchmark->perf#/X bm (lambda (a b) (* 2 (stddev/mean a b)))))

(define (benchmark->perf#/variance bm)
  (benchmark->perf#/X bm variance/mean))

(define (benchmark->perf#/t99 bm)
  (benchmark->perf#/X bm (t-test #:alpha 0.01 #:two-sided? #true)))

(define (benchmark->perf#/t95 bm)
  (benchmark->perf#/X bm (t-test #:alpha 0.05 #:two-sided? #true)))

(define (benchmark->perf#/t90 bm)
  (benchmark->perf#/X bm (t-test #:alpha 0.10 #:two-sided? #true)))

(define (benchmark->perf#/t80 bm)
  (benchmark->perf#/X bm (t-test #:alpha 0.20 #:two-sided? #true)))

(define (benchmark->perf#/X bm fx)
  (define in-file (build-path runtime-data-dir (format "~a.rktd" bm)))
  (define vv (file->value in-file))
  (define ut* (cadr (car vv)))
  (for/hash ((row (in-list vv)))
    (define t* (cadr row))
    (define o* (map / t* ut*))
    (define mm (mean o*))
    (define ss (fx mm o*))
    #; (printf "   val ~s~n" (shortnum ss))
    (values (car row) (map shortnum (list mm ss)))))

(define (benchmark->raw# bm)
  (define in-file (build-path runtime-data-dir (format "~a.rktd" bm)))
  (define vv (file->value in-file))
  (define ut* (cadr (car vv)))
  (for/hash ((row (in-list vv)))
    (values (car row) (cadr row))))

(define ((t-test #:alpha [alpha 0.05] #:two-sided? [two-sided? #true]) mm n*)
  ;; gbe-oopsla-2007 sec 3,2,2
  (define N (length n*))
  (define degrees-of-freedom (sub1 N))
  (unless (<= N 30)
    (raise-argument-error 't-test "too many samples"))
  (define ss (stddev/mean mm n*))
  (define t-factor (lookup-t alpha degrees-of-freedom #:two-sided? two-sided?))
  (define t-offset (* t-factor (/ ss (sqrt N))))
  t-offset
  #;(list (- mm t-offset)
        (+ mm t-offset)))

(define (lookup-t alpha deg #:two-sided? [two-sided? #true])
  ;; https://en.wikipedia.org/wiki/Student%27s_t-distribution
  (define confidence-level (- 1 (if two-sided? alpha (/ alpha 2))))
  (cond
    ((= deg 7)
     (cond
       (two-sided?
        (case alpha
          ((0.01) 3.499) ;; 99%
          ((0.05) 2.365) ;; 95%
          ((0.10) 1.895) ;; 90%
          ((0.20) 1.415) ;; 80%
          (else (raise-user-error 'dieee))))
       (else
        (case alpha
          ((0.01) 2.998) ;; 99%
          ((0.05) 1.895) ;; 95%
          ((0.10) 1.415) ;; 90%
          ((0.20) 0.896) ;; 80%
          (else (raise-user-error 'dieee))))))
    (else
     (raise-argument-error 'degrees-freedom "die"))))

(define benchmark->perf#
  benchmark->perf#/stddev)

(module+ main
  (require racket/cmdline racket/pretty)
  (command-line
    #:args args
    (if (null? args)
      (printf "usage: racket runtime.rkt [bm] [cfg]~n")
      (let* ((bm (car args))
             (h (benchmark->perf# bm)))
        (if (null? (cdr args))
          (pretty-write h)
          (displayln (hash-ref h (cadr args) #f)))))))

