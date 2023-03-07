#lang racket

(require
  math/statistics
  file/glob text-table)

;; TODO
;; - [X] fname -> bmname + strategy + b/p/p
;; - [ ] group by bname,
;;   - find global wins, across strategies
;;   - find local wins, within strategy

(define (tt xx)
  #;(pretty-write xx)
  (printf "mws = mean win steps = on average, how many steps to reach success?~n")
  (printf "mlo = mean loss overhead = on average, how slow at a stuck state?~n")
  (newline)
  (displayln (simple-table->string xx #:align '(left left right))))

(struct rktd [name path bm strat mode] #:prefab)
(struct row [cfg end ms ns ss] #:prefab)
(struct trail [success configs mstep mfail] #:prefab)

(define (all-rktd dd)
  (for/list ((pp (in-list (glob (build-path dd "*rktd")))))
    (define name (fname pp))
    (define-values [bm strat mode] (split-filename name))
    (rktd name pp bm strat mode)))

(define (hyphen-split str)
  (string-split str "-"))

(define (hyphen-join str*)
  (string-join str* "-"))

(define (split-filename str)
  (define elem* (hyphen-split (car (string-split str "."))))
  (define bm (first elem*))
  (define mode (last elem*))
  (define strat (hyphen-join (drop-right (cdr elem*) 1)))
  (values bm strat mode))

(define (pct a b)
  (format "~a%" (exact-round (* 100 (/ a b)))))

(define title* (list "benchmark" "strategy" "boundary" "prf_self" "prf_total" "N configs"))

(define (empty-row)
  (make-list (length title*) ""))

(define (add-blanks tt)
  (add-between tt (empty-row)))

(define (go dd)
  (define seen (make-hash))
  (tt (cons title* (rktd->rows (all-rktd dd) seen))))

(define (rktd->rows rktd* seen)
  (apply append
   (for/list ((bm* (in-list (group-by rktd-bm rktd*))))
     (cons
       (empty-row)
       (benchmark-rktd*->rows bm* seen)))))

(define (benchmark-rktd*->rows bm* seen)
  (apply append
   (for/list ((strat* (in-list (group-by rktd-strat bm*))))
    (strat-rktd*->rows strat* seen))))

(define (strat-rktd*->rows strat* seen)
    (define name
      (let ((nn (rktd-bm (car strat*))))
        (if (hash-ref seen nn #f)
          ""
          (begin (hash-set! seen nn #true) nn))))
    (define strat (rktd-strat (car strat*)))
    (define wt*
      (for/list ((rr (in-list strat*)))
        (define wt (num-success (rktd-path rr)))
        #;(printf "~s ~s ~s = ~s~n" (rktd-bm rr) (rktd-strat rr) (rktd-mode rr) wt)
        wt))
    (define total
      (let ((n* (map trail-configs wt*)))
        (if (apply = n*)
          (car n*)
          (begin (printf "ERROR unequal totals~n ~s~n ~s~n" n* (map rktd-path strat*))
                 (apply max n*)))))
    (list
      ;; success row
      (append (list name strat)
              (for/list ((w (in-list (map trail-success wt*)))) (pct w total))
              (list total))
      ;; steps, failures
      (append (list "" "")
              (for/list ((wt (in-list wt*))) (format "~amws ~amlo" (trail-mstep wt) (trail-mfail wt)))
              (list ""))))

(define (fname xx)
  (path->string (file-name-from-path xx)))

(define (rnd n)
  (if (string? n)
    n
    (~r n #:precision '(= 1))))

(define (mean* xx)
  (if (null? xx)
    "N/A"
    (mean xx)))

(define (num-success fn)
  (define fval (file->value fn))
  (define u-ms (row-ms (car fval)))
  (define (overhead rr)
    (/ (row-ms rr) u-ms))
  (define (fast-enough? rr)
    (< (overhead rr) 2))
  (define (steps rr)
    (sub1 (row-ns rr)))
  (for/fold ([win 0]
             [all 0]
             [ws '()]
             [lo '()]
             #:result (trail win all (rnd (mean* ws)) (rnd (mean* lo))))
            ((rr (in-list fval)))
    (define v (row-end rr))
    (define win?  (or (and (pair? v) (eq? 'success (car v)))
                      (fast-enough? rr)))
    (define los? (not win?))
    (values (+ win (if win? 1 0))
            (+ all 1)
            (if win? (cons (steps rr) ws)    ws)
            (if los? (cons (overhead rr) lo) lo))))

(module+ main
  (require racket/cmdline)
  (command-line
    #:args (dd)
    (go dd)))


