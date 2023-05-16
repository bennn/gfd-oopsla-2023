#lang racket

;; Use the t-test, are untyped and deep-typed from "same" distro?

(require
  "base.rkt"
  "runtime.rkt"
  t-test)
;; <https://docs.racket-lang.org/t-test/index.html>

(define (fmt n)
  (exact-round (* 100 n)))

(define (tcheck s0 s1)
  `((student ,(fmt (student-t-test s0 s1)))
    (welch ,(fmt (welch-t-test s0 s1)))))

(define bm* (all-benchmark-name*))

(for ((bm (in-list bm*)))
  (define vv (file->value (format "runtime/~a.rktd" bm)))
  (define ut* (cadr (car vv)))
  (define dt* (for/first ((row (in-list (cdr vv)))
                          #:when (for/and ((cc (in-string (car row)))) (eq? #\1 cc)))
                (cadr row)))

  (printf "~a ~a~n" bm (tcheck ut* dt*)))


