#lang racket/base

(provide
  all-benchmark-name*
  all-strategy-name*
  all-mode-name*
  bm->num-configs
  ensure-dir
  (struct-out rktd)
  (struct-out row)
  (struct-out trail)
  hyphen-split
  hyphen-join
  split-filename
  hash-add1
  hash-add1!)

(require
  racket/list
  racket/string
  racket/file)

;; ===

(struct rktd [name path bm strat mode] #:prefab)
(struct row [cfg end ms ns ss] #:prefab)
(struct trail [success configs mstep mfail] #:prefab)

(define (all-benchmark-name*)
  ;; TODO sort by string name too, lex[num, str]
  (map car (sort (file->value "data/good-cfgs.rktd") < #:key third)))

(define (hyphen-split str)
  (string-split str "-"))

(define (hyphen-join str*)
  (string-join str* "-"))

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
  '("con"
    "cost-con" "cost-opt"
    "limit-con" "limit-opt"
    "opt"
    "randomD" "randomS"
    "toggleD" "toggleS"))

(define (all-mode-name*)
  '("boundary" "prf_total" "prf_self"))

