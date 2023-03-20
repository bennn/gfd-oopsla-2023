#lang racket/base

(require
  math/statistics
  racket/list
  racket/string
  racket/format
  racket/file)

(define (go)
  (define data*
    (sort (file->value "data/good-cfgs.rktd") < #:key third))
  (define tt
    (for/list ((row (in-list data*)))
      (define name (first row))
      (define num-good (second row))
      (define num-cfgs (third row))
      (define num-bad (- num-cfgs num-good))
      (list name
            num-bad
            (pct num-bad num-cfgs))))
  (define title* '("Benchmark" "Count" "Pct"))
  (tex-table title* tt #:avg? #true))

(define (pct a b)
  (* 100 (/ a b)))

(define (rnd n)
  (cond
    ((exact-integer? n)
    (number->string n))
    ((real? n)
    (string-append (~r n #:precision '(= 2)) "\\%"))
    (else (~a n))))

(define (row-join str*)
  (string-join str* " & "))

(define (tex-table title* tt #:avg? [avg? #f])
  (printf "  \\begin{tabular}{l~a}~n" (make-string (length (cdr title*)) #\r))
  (printf "    ~a \\\\\\midrule~n" (row-join title*))
  (for ((rr (in-list tt)))
    (printf "    ~a \\\\~n" (string-join (map rnd rr) " & ")))
  (when avg?
    (printf "    Total & ~a & ~a \\\\~n"
            (rnd (apply + (map second tt)))
            (rnd (median < (map third tt)))))
  (printf "  \\end{tabular}~n")
  (void))

(module+ main (go))
