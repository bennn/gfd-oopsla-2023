#lang racket/base

(require
  math/statistics
  (only-in math/number-theory binomial)
  racket/list
  racket/string
  racket/format
  racket/file
  pict pict-abbrevs
  plot/no-gui
  (except-in plot/utils min* max*))

(define (f:where-trouble)
  (define by-level*
    (let* ((vv (file->value "data/t1-good-by-level.rktd"))
           (vv (for/hash ((nl (in-list vv))) (values (first nl) (second nl)))))
      (for/list ((bm+good+all (in-list (good-cfgs))))
        (define name (first bm+good+all))
        (define total (third bm+good+all))
        (define modules (int-cube total))
        (define pcts
          (for/list ((lvl (in-range (+ 1 modules))))
            (define good-count
              (cdr
                (or (assoc lvl (hash-ref vv name))
                    (cons lvl 0))))
            (define all-count
              (let* ((num-at-level-for-2n (binomial modules lvl)))
                (* num-at-level-for-2n (expt 2 lvl))))
            (define bad-count (- all-count good-count))
            (define pp (pct bad-count total))
            (list lvl pp)))
        (list name pcts))))
  (save-pict
    "data/where-trouble.pdf"
    (parameterize ( )
      (define *ymax (box 0))
      (ptable
        #:ncol 3
        #:row-sep 8
        #:col-sep 10
        (cons (blank)
        (for/list ((name+lvl* (in-list by-level*)))
          (define name (first name+lvl*))
          (define lvl* (second name+lvl*))
          (plot-pict
            (rectangles
              (for/list ((lvl (in-list lvl*))
                         #:when (< 0 (second lvl)))
                (define xx (first lvl))
                (define yy (second lvl))
                (set-box! *ymax (max yy (unbox *ymax)))
                (vector (ivl (- xx 1/4) (+ xx 1/4))
                        (ivl 0 yy)))
              #:line-color 1
              #:color 1)
            #:title (format "~a" name)
            #:x-label #f
            #:y-label #f
            #:x-min 0
            #:x-max (- (length lvl*) 1/2)
            #:y-min 0
            #:y-max (unbox *ymax)
            #:width  100
            #:height  80)))))))

(define (t:baseline-trouble)
  (define data* (good-cfgs))
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

(define (int-cube n)
  (let loop ((k 1))
    (if (= n (expt 3 k))
      k
      (loop (+ k 1)))))

(define (good-cfgs)
  (sort (file->value "data/good-cfgs.rktd") < #:key third))

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

;; ===

(define (go)
  (t:baseline-trouble)
  (f:where-trouble))

(module+ main (go))
