#lang racket

(require
  "base.rkt"
  "runtime.rkt"
  math/statistics)

(define (rnd n)
  (~r n #:precision '(= 2)))

(for ((bm (in-list (all-benchmark-name*))))
  (define perf# (benchmark->perf# bm))
  (define n* (map car (hash-values perf#)))
  (printf "~a : wc ~ax mean ~a~n" bm (rnd (apply max n*)) (rnd (mean n*)))
  (void))


