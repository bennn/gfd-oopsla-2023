#lang racket

(define (go)
  (define num-configs (apply + (map third (file->value "data/good-cfgs.rktd"))))
  (define num-runtime-warmup-runs 1)
  (define num-runtime-real-runs 8)
  (define num-runtime-runs (+ num-runtime-warmup-runs num-runtime-real-runs))
  (define num-boundary-runs 1)
  (define num-statistical-runs 1)
  (define num-runs (+ num-runtime-runs num-boundary-runs num-statistical-runs))
  (printf " ~a configs, ~a runs~n" num-configs (* num-runs num-configs)))

(module+ main (go))

