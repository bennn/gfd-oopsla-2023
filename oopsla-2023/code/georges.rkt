#lang racket

(require
  "base.rkt"
  "runtime.rkt"
  (only-in math/statistics mean stddev/mean)
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

(define (pct num-good total)
  (exact-round (* 100 (/ num-good total))))

(define (go-perf bm bm->pp)
  (define perf# (bm->pp bm))
  (printf " ~a // ~a~n" bm (object-name bm->pp))
  (printf "  typed good? ~a~n" (good-overhead? (typed-val perf#)))
  (define num-good (for/sum ((vv (in-hash-values perf#))) (if (good-overhead? vv) 1 0)))
  (define total (hash-count perf#))
  (define pct-good (pct num-good total))
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

(define (go-filter bm# ff)
  (define rr
    (for/hash (((k v) (in-hash bm#))
               #:when (ff v))
      (values k v)))
  (define olde (hash-count bm#))
  (define newe (hash-count rr))
  (printf " ~a / ~a = ~a%~n" newe olde (pct newe olde))
  (void))

(define (ms-gap bm* mm)
  (printf "=== ms-gap ~s~n" mm)
  (for ((bm (in-list bm*)))
    (define pp (benchmark->raw# bm))
    (printf "~a~n" bm)
    (define u-t* (untyped-val pp))
    (define mu (mean u-t*))
    (define su (stddev/mean mu u-t*))
    (define (reject-but-tiny? t*)
      (define o* (map / t* u-t*))
      (define mean-o (mean o*))
      (define std-o (stddev/mean mean-o o*))
      (and (not (good-overhead? (map shortnum (list mean-o std-o))))
           (<= (abs (- mu (mean t*))) mm)))
    (define (tiny-mean? t*)
      (define bb (mean t*))
      (<= (abs (- mu bb)) mm))
    #;(define (any-tiny-gap? t*)
      (for/or ((a (in-list u-t*))
               (b (in-list t*)))
        (define tg (<= (abs (- a b)) mm))
        (if tg
          (begin #;(printf "  tiny gap ~a ~a~n" a b)
          #true)
          #false)))
    (go-filter pp tiny-mean?)
    (void)))

(define (unconverged bm*)
  (printf "=== unconverged~n")
  (for ((bm (in-list bm*)))
    (define pp (benchmark->raw# bm))
    (printf "~a~n" bm)
    (go-filter pp (compose1 not converged?))
    (void)))

;; 95 CI within 10% of sample mean
;; TODO what about nonparametric stats?
(define (converged? t*)
  (define mm (mean t*))
  (define m10 (* 0.10 mm))
  (define ci ((t-test) mm t*))
  (define rr (<= ci m10))
  (define upper (* 0.35 mm))
  (or (<= ci m10)
      (begin
        (unless (<= ci upper) (printf "CI ~s~n MM ~s~n" ci upper)) #f)))

(module+ main
  (define bm*
    ;; '(forth mbta dungeon zombie take5 acquire) ;; all somewhat hopeless
    #;(rtake 6 (all-benchmark-name*))
    (all-benchmark-name*))
  #;(go bm*)
  #;(low-hi bm*)
  ;; (ms-gap bm* 100)
  ;; (newline)
  (unconverged bm*)
  )


