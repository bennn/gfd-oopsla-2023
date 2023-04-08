#lang racket/base

(provide
  all-benchmark-name*
  benchmark-index
  config->deep
  config->shallow
  config-sort
  config-reachable?
  all-strategy-name*
  all-mode-name*
  point-sym*
  take-some
  row*->get-overhead
  ms->get-overhead
  row-trail-success?
  row-trail-failure?
  bm->num-configs
  good-overhead?
  overhead<=?
  overhead>?
  pct2 pctstr2
  pct pctstr
  ensure-dir
  (struct-out rktd)
  ;;(struct-out row)
  (struct-out trail)
  hyphen-split
  hyphen-join
  tex-row-join
  split-filename
  hash-add1
  hash-add1!)

(require
  racket/math
  racket/list
  racket/string
  racket/format
  racket/file)

;; ===

(struct rktd [name path bm strat mode] #:prefab)
(struct row [cfg end ms ns ss] #:prefab)
;; TODO refactor row struct

(struct trail [success configs mstep mfail] #:prefab)

(define all-benchmark-name*
  (let ((*cache (box #f)))
    (lambda ()
      (or (unbox *cache)
          (let ((vv (map car (sort (file->value "data/good-cfgs.rktd") < #:key third))))
            ;; TODO sort by string name too, lex[num, str]
            (set-box! *cache vv)
            vv)))))

(define (benchmark-index str)
  (define key (->symbol str))
  (index-of (all-benchmark-name*) key))

(define (->symbol xx)
  (cond
    ((symbol? xx)
     xx)
    ((string? xx)
     (string->symbol xx))
    (else
      (raise-argument-error '->symbol "oops" xx))))

(define (hyphen-split str)
  (string-split str "-"))

(define (hyphen-join str*)
  (string-join str* "-"))

(define (tex-row-join str*)
  (string-join str* " & "))

(define (split-filename str)
  (define elem* (hyphen-split (car (string-split (path-string->string str) "."))))
  (define bm (first elem*))
  (define mode (last elem*))
  (define strat (hyphen-join (drop-right (cdr elem*) 1)))
  (values bm strat mode))

(define (path-string->string pp)
  (if (path? pp)
    (path->string pp)
    (if (string? pp)
      pp
      (raise-argument-error 'path-string->string "path-string?" pp))))

(define (fzero) 0)

(define (hash-add1! h k)
  (hash-update! h k add1 fzero))

(define (hash-add1 h k)
  (hash-update h k add1 fzero))

(define (ensure-dir str)
  (unless (directory-exists? str)
    (make-directory str)))

(define bm->num-configs
  (let ((cache (box #f)))
    (lambda (-name)
      (define bm-name (if (string? -name) (string->symbol -name) -name))
      (define data* (or (unbox cache)
                        (let ((vv (file->value "data/good-cfgs.rktd")))
                          (set-box! cache vv)
                          vv)))
      (for/first ((row (in-list data*))
                  #:when (eq? bm-name (first row)))
        (third row)))))

(define (all-strategy-name*)
  ;; TODO compute from directory
  '("opt"
    "cost-opt"
    "limit-con"
    "con"
    "cost-con"
    "limit-opt"
    "randomD"
    "randomS"))

(define (all-mode-name*)
  '("boundary" "prf_total" "prf_self"))

(define (take-some x*)
  (take x* 2))

(define (row*->get-overhead rr*)
  (define urow (car rr*))
  (define utime (row-ms urow))
  (ms->get-overhead utime))

(define (ms->get-overhead utime)
  (lambda (xx) (/ xx utime)))

(define (row-trail-success? rr)
  (eq? 'success (car (row-end rr))))

(define (row-trail-failure? rr ovr)
  #;(eq? 'error (car (row-end rr)))
  (< 1 (ovr (row-ms rr))))

(define (pct aa bb)
  (exact-round (pct2 aa bb)))

(define (pctstr aa bb)
  (format "~a%" (pct aa bb)))

(define (pct2 aa bb)
  (* 100 (/ aa bb)))

(define (pctstr2 aa bb)
  (format "~a%" (~r (pct2 aa bb) #:precision '(= 2))))

(define point-sym* '(
  plus
  triangle triangledown
  5asterisk 8star
  circle
  square
  diamond
  times
  6star
  bullet
  asterisk
  pixel
  circle7
  ))

(define (config-sort cfg*)
  ;; TODO right order?
  (sort cfg* string<?))

(define (good-overhead? m/s)
  (<= (apply - m/s) 1))

(define (overhead<=? a b)
  (not (overhead>? a b)))

(define (overhead>? a b)
  (> (apply - a)
     (apply + b)))

(define (config->deep str)
  (string-replace* str #\2 #\1))

(define (config->shallow str)
  (string-replace* str #\1 #\2))

(define (string-replace* str from to)
  (apply string
         (for/list ((cc (in-string str)))
           (if (eq? cc from) to cc))))

(define (config-reachable? c0 c1)
  (unless (= (string-length c0)
             (string-length c1))
    (raise-argument-error 'config-reachable? "length mismatch" c0 c1))
  (for/and ((bit0 (in-string c0))
            (bit1 (in-string c1)))
    (or (untyped-bit? bit0)
        (not (untyped-bit? bit1)))))

(define (untyped-bit? cc)
  (eq? #\0 cc))


