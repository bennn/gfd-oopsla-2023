#lang racket

;; TODO wiggle room in data, don't use means alone!!!

(require
  "base.rkt"
  "runtime.rkt"
  file/glob
  text-table
  racket/runtime-path)

;; ---

(define-runtime-path trail-dir "../t1-img/")

(struct row (cfg end cc*) #:prefab)
;; TODO delete

(struct traildata (bb ss mm ff) #:transparent)
(struct bmres (bb seascape trails) #:transparent)
(struct seascape (num-configs immediate# hopeful# hopeless#) #:prefab)

(define (all-trail-data)
  (for/list ((fn (in-glob (build-path trail-dir "*rktd"))))
    (define-values [bm strategy mode] (split-filename (file-name-from-path fn)))
    (traildata bm strategy mode fn)))

(define (go)
  (define data* (all-trail-data))
  (define bm** (sort (filter-not null? (group-by traildata-bb data*))
                     < #:key (compose1 benchmark-index traildata-bb car)))
  (define res** (map go-bm (take-some bm**)))
  (with-output-to-file "tmp-success.rktd" #:exists 'replace (lambda () (pretty-write res**)))
  (define tbl* (combine res**))
  (void))

(define (take-some x*)
  ;; TODO
  (values x*)
  #;(take x* 4))

(define (go-bm bm*)
  ;; forall benchmark, split configs into: immediate, hopeless, hopeful
  ;; forall strategy + mode, forall hopeful: 
  (define bm-name (traildata-bb (car bm*)))
  (define perf# (benchmark->perf# bm-name))
  (define ss (bm-seascape perf#))
  (define tt (bm-trails bm* perf# ss))
  (bmres bm-name ss tt))

(define (untyped-ms perf#)
  (hash-ref perf# (untyped-config perf#)))

(define (untyped-config perf#)
  (define num-bits
    (or (for/first ((k (in-hash-keys perf#)))
          (string-length k))
        (error 'emptyperf)))
  (make-string num-bits #\0))

(define (bm-seascape perf#)
  (define num-configs (hash-count perf#))
  (define ovr (ms->get-overhead (untyped-ms perf#)))
  (define (fast-enough? cfg perf)
    (or (good-overhead? (ovr perf))
        (good-overhead? (ovr (hash-ref perf# (config->deep cfg))))
        (good-overhead? (ovr (hash-ref perf# (config->shallow cfg))))))
  (define-values [immediate# other#]
    (for/fold ((imm# (hash))
               (oth# (hash))
               #:result (values imm# oth#))
              (((cfg perf) (in-hash perf#)))
      (if (fast-enough? cfg perf)
        (values (hash-set imm# cfg #true) oth#)
        (values imm# (hash-set oth# cfg #true)))))
  (define-values [hopeful# hopeless#]
    (for/fold ((good# (hash))
               (bad# (hash))
               #:result (values good# bad#))
              ((cfg (in-hash-keys other#)))
      (if (reachable? cfg immediate#)
        (values (hash-set good# cfg #true) bad#)
        (values good# (hash-set bad# cfg #true)))))
  (seascape num-configs immediate# hopeful# hopeless#))

(define (reachable? cfg tgt#)
  (for/or ((tgt (in-hash-keys tgt#)))
    (config-reachable? cfg tgt)))

(define (bm-trails bm* perf# ss)
  (define win# (seascape-immediate# ss))
  (define hope# (seascape-hopeful# ss))
  (define num-hopeful (hash-count hope#))
  (for/list ((td (in-list bm*)))
    (define ss (traildata-ss td))
    (define mm (traildata-mm td))
    (define-values [mono-win any-win]
      (for/fold ((num-mono 0)
                 (num-any 0)
                 #:result (values num-mono num-any))
                ((rr (in-list (file->value (traildata-ff td))))
                 #:when (hash-ref hope# (row-cfg rr) #f))
        (define cc* (row-cc* rr))
        (define win? (and (not (null? cc*)) (hash-ref win# (last cc*) #f)))
        (define mono? (and win?
                           (let loop ((perf (hash-ref perf# (row-cfg rr)))
                                      (cc cc*))
                             (if (null? cc)
                               #true
                               (let ((next (hash-ref perf# (car cc))))
                                 (and (<= next perf)
                                      (loop next (cdr cc))))))))
        (values (+ (if mono? 1 0) num-mono)
                (+ (if win? 1 0) num-any))))
    (list ss
          mm
          mono-win
          any-win
          num-hopeful)))

(define (combine res**)
  (displayln
    (simple-table->string
      #:align '(left right)
      (cons
        (list "benchmark" "%easy" "%hopeless" "%ok" "3^N")
        (for/list ((res (in-list res**)))
          (define bb (bmres-bb res))
          (define ss (bmres-seascape res))
          (define nc (seascape-num-configs ss))
          (define nw (hash-count (seascape-immediate# ss)))
          (define ny (hash-count (seascape-hopeful# ss)))
          (define nn (hash-count (seascape-hopeless# ss)))
          (list bb (pctstr nw nc) (pctstr nn nc) (pctstr ny nc) nc)))))
  (newline)
  (define allrow*
        (for/list ((strat (in-list (all-strat res**)))
                   (mode (in-list (all-mode res**))))
          (define-values [num-mono num-stubborn num-total]
            (for*/fold ((nm 0)
                       (ns 0)
                       (nt 0)
                       #:result (values nm ns nt))
                      ((rr (in-list res**))
                       (tt (in-list (bmres-trails rr)))
                       #:when (and (equal? (first tt) strat)
                                   (equal? (second tt) mode)))
              (values (+ nm (third tt))
                      (+ ns (fourth tt))
                      (+ nt (fifth tt)))))
          (list strat
                mode
                (pctstr num-mono num-total)
                (pctstr num-stubborn num-total)
                num-total)))
  (define row** (group-by second allrow*))
  (for ((row* (in-list row**)))
    (displayln
      (simple-table->string
        #:align '(left right)
        (cons
          (list "strategy" "mode" "%mono win" "%stubborn win" "total scenarios")
          row*)))
    (newline))
  (void))

(define (all-strat res*)
  #;("opt"
    "cost-opt"
    "limit-con"
    "con"
    "cost-con"
    "limit-opt"
    "randomD"
    "randomS")
  (res-get res* first))

(define (all-mode res*)
  (res-get res* second))

(define (res-get res* ff)
  (define rr (car res*))
  (for/list ((tt (in-list (bmres-trails rr))))
    (ff tt)))

(module+ main (go))

