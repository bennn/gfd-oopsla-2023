#lang racket

(require
  "base.rkt"
  "runtime.rkt"
  )

(define (untyped-val h#)
  (define k (untyped-key h#))
  (hash-ref h# k))

(define (untyped-key h#)
  (define k0 (or (for/first ((kk (in-hash-keys h#))) kk) (error 'emptyhash)))
  (make-string (string-length k0) #\0))

(define (typed-val h#)
  (define k (typed-key h#))
  (hash-ref h# k))

(define (typed-key h#)
  (define k0 (or (for/first ((kk (in-hash-keys h#))) kk) (error 'emptyhash)))
  (make-string (string-length k0) #\1))

(define (before bm)
  (go-perf bm benchmark->perf#))

(define (go-perf bm bm->pp)
  (define perf# (bm->pp bm))
  (printf " ~a // ~a~n" bm (object-name bm->pp))
  (printf "  typed good? ~a~n" (good-overhead? (typed-val perf#)))
  (define num-good (for/sum ((vv (in-hash-values perf#))) (if (good-overhead? vv) 1 0)))
  (define total (hash-count perf#))
  (define pct-good (exact-round (* 100 (/ num-good total))))
  (printf "  num good = ~a (~a%)~n"
          num-good pct-good)
  (define num-backup (for/sum ((vv (in-hash-values perf#))) (if (backup-overhead? vv) 1 0)))
  (define pct-backup (exact-round (* 100 (/ num-backup total))))
  (printf "  num backup = ~a (~a%)~n"
          num-backup pct-backup)
  (void))

(define (after bm)
  (define ff* (list
    benchmark->perf#/stddev
    #;benchmark->perf#/2stddev
    #;benchmark->perf#/variance
    #;benchmark->perf#/t99
    #;benchmark->perf#/t95
    #;benchmark->perf#/t90
    #;benchmark->perf#/t80))
  (for ((ff (in-list ff*)))
    (go-perf bm ff))
  (newline)
  (void))

(define (go bm*)
  (printf "georges checkup~n")
  ;; (printf "=== before~n")
  ;; (for-each before bm*)
  (printf "~n=== after~n")
  (for-each after bm*)
  (printf "~ndone~n")
  (void))

(define (low-hi bm*)
  (for ((bm (in-list bm*)))
    (define pp (benchmark->perf# bm))
    (define overhead (car (typed-val pp)))
    (printf "~a : ~a~n" bm (~r #:precision '(= 2) overhead))
    (void)))

(define (rtake a b) (take b a))

(module+ main
  (define bm*
    '(forth mbta dungeon zombie take5 acquire) ;; all somewhat hopeless
    #;(rtake 6 (all-benchmark-name*))
    #;(all-benchmark-name*))
  (go bm*)
  #;(low-hi bm*))


