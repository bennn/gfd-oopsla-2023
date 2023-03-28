#lang racket

(require
  "base.rkt")

;; ---

(define (endpoints bm)
  (define rr* (file->value "t1-img/snake-opt-boundary.rktd"))
  (define h# (make-hash))
  (for ((rr (in-list rr*))
        #:when (row-trail-failure? rr))
    (define trail (row-ss rr))
    (define end (last trail))
    (hash-add1! h# end))
  (define h* (hash->list h#))
  (pretty-write (sort h* > #:key cdr))
  (printf " ~a total~n" (apply + (map cdr h*))))

(module+ main (endpoints 'snake))

