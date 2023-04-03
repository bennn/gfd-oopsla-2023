#lang racket

;; 31 self win       27 total win   9 mixed    69 tie
;; 78 boundary win   11 total win   24 mixed   23 tie


(require
  "base.rkt"
  file/glob
  racket/pretty
  text-table
  pict pict-abbrevs
  plot/no-gui
  (except-in plot/utils min* max*))

;; ---

(define out-kind
  #;'pdf
  'png)

(define (t:start-vs-end bm**)
  (printf "-100x  -10x  -2x  0   2x   10x   100x~n")
  (define tbl**
    (for/list ((bm* (in-list bm**)))
      (for/list ((bm (in-list bm*)))
        (define nn (short-name (file-name-from-path bm)))
        (cons nn (bucket-nums bm)))))
  #;(vs-match-up tbl** self-row? total-row?)
  (vs-match-up tbl** boundary-row? total-row?)
  #;(for ((tbl* (in-list tbl**)))
    #;(printf "\\begin{tabular}{lcccccc}~n")
    (displayln
      (simple-table->string #:align '(left right) tbl*))
    (newline)
    (void))
  (void))

(define (pair-up xx*0)
  (let loop ((xx* xx*0))
    (if (null? xx*)
      '()
      (cons (list (car xx*) (cadr xx*)) (loop (cddr xx*))))))

(define (vs-match-up tbl** p0 p1)
  (define self-win 0)
  (define total-win 0)
  (define unclear-win 0)
  (define no-win 0)
  (for ((tbl* (in-list tbl**)))
    (for ((rr (in-list (pair-up tbl*))))
      (define row0 (car rr))
      (define row1 (cadr rr))
      (unless (p0 row0)
        (error 'die-notself))
      (unless (p1 row1)
        (error 'die-nottotal))
      (case
        (row-compare row0 row1)
        ((-2) (set! self-win (+ 1 self-win)))
        ((+2) (set! total-win (+ 1 total-win)))
        ((-1 1) (set! unclear-win (+ 1 unclear-win)))
        (else (set! no-win (+ 1 no-win))))))
  (printf " ~a ~a win ~a ~a win ~a mixed ~a tie~n"
          self-win p0
          total-win p1
          unclear-win no-win)
  (void))

(define (pctnum* xx)
  (map pctnum xx))

(define (pctnum xx)
  (string->number (substring xx 0 (- (string-length xx) 1))))

(define (row-compare row0 row1)
  (define nn0 (pctnum* (cdr row0)))
  (define nn1 (pctnum* (cdr row1)))
  (cond
    [(andmap = nn0 nn1)
     0]
    [(row-better? nn0 nn1)
     -2]
    [(row-better? nn1 nn0)
     2]
    [else
      1]))

(define (row-better? r0 r1)
  (define pre0 (take r0 3))
  (define pre1 (take r1 3))
  (define pos0 (drop r0 4))
  (define pos1 (drop r1 4))
  (and (andmap <= pre0 pre1)
       (andmap >= pos0 pos1)))

(define (boundary-row? rr)
  (string-match-row? rr "-boundary"))

(define (self-row? rr)
  (string-match-row? rr "-prf_self"))

(define (total-row? rr)
  (string-match-row? rr "-prf_total"))

(define (string-match-row? rr tgt)
  (define str (car rr))
  (string-contains? str tgt))

(define (short-name fname)
  (car (string-split (path->string fname) ".")))

(define (bucket-nums bm)
  (define-values [bmname _ss _mm] (split-filename (file-name-from-path bm)))
  (define num-configs (bm->num-configs bmname))
  (define (pctcfg kk)
    (format "~a%" (pct kk num-configs)))
  (define h* (hash-int-list (file->value bm)))
  (for/list ((nn (in-list '(-100 -10 -1 0 1 10 100))))
    (define cmp
      (if (< nn 0)
        (lambda (kk) (and (< kk 0) (<= kk nn)))
        (lambda (kk) (and (< 0 kk) (<= nn kk)))))
    (pctcfg
      (if (zero? nn)
        (- num-configs (apply + (map cdr h*)))
        (for/sum ((kv (in-list h*))
                  #:when (cmp (car kv)))
          (cdr kv))))))

(define (hash-int-list h#)
  (sort (hash->list h#) < #:key car))

(define (f:start-vs-end bm*)
  (define pp (error 'die))
  (save-pict (format "data/start-vs-end.~a" out-kind) pp)
  (void))

(define (all-data-file*)
  (for/list ((bm (in-list (all-benchmark-name*))))
    (filter yes-care? (glob (build-path "data" "start-vs-end" (format "~a-*.rktd.hash" bm))))))

(define (yes-care? ps)
  (not (dont-care ps)))

(define (dont-care ps)
  (define-values [bm ss mm] (split-filename (file-name-from-path ps)))
  (or (string-prefix? ss "toggle")
      #;(string-prefix? mm "boundary")
      (string-prefix? mm "prf_self")
      #;(and (string-prefix? ss "random")
           (string-prefix? mm "prf"))))

(define (go)
  (define bm* (all-data-file*))
  (t:start-vs-end bm*)
  #;(f:start-vs-end bm*)
  (void))

(module+ main
  (go))

