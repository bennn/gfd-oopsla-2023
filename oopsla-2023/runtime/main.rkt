#lang racket/base

(provide
  benchmark->perf#)

(require
  (only-in racket/file file->value)
  (only-in math/statistics mean)
  racket/runtime-path)

(define-runtime-path pwd ".")

(define (benchmark->perf# bm)
  (define in-file (build-path pwd (format "~a.rktd" bm)))
  (for/hash ((row (in-list (file->value in-file))))
    (values (car row) (mean (cadr row)))))

