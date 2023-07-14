#lang racket/base

(provide
  benchmark->perf#)

(require
  (only-in racket/file file->value)
  (only-in math/statistics mean stddev/mean)
  (only-in racket/math exact-round)
  racket/runtime-path)

(define-runtime-path runtime-data-dir "./runtime/")

(define ww 100)

(define (shortnum n)
  (/ (exact-round (* n ww)) ww))

(define (benchmark->perf# bm)
  (define in-file (build-path runtime-data-dir (format "~a.rktd" bm)))
  (define vv (file->value in-file))
  (define ut* (cadr (car vv)))
  (for/hash ((row (in-list vv)))
    (define t* (cadr row))
    (define o* (map / t* ut*))
    (define mm (mean o*))
    (define ss (stddev/mean mm o*))
    (values (car row) (map shortnum (list mm ss)))))

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


