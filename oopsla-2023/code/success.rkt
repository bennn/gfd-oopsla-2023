#lang racket

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

(struct trailfile (bb ss mm ff) #:transparent)
(struct trailres (ss mm win-0 win-1 win-2 win-3 win-N num-feasible) #:prefab)
(struct bmres (bb seascape trails) #:transparent)
(struct seascape (num-configs immediate# feasible# hopeless#) #:prefab)

(define (all-trail-data)
  (for/list ((fn (in-glob (build-path trail-dir "*rktd"))))
    (define-values [bm strategy mode] (split-filename (file-name-from-path fn)))
    (trailfile bm strategy mode fn)))

(define (go)
  (define data* (all-trail-data))
  (define bm** (sort (filter-not null? (group-by trailfile-bb data*))
                     < #:key (compose1 benchmark-index trailfile-bb car)))
  (define res** (map go-bm (take-some bm**)))
  (with-output-to-file "data/success-res.rktd" #:exists 'replace (lambda () (pretty-write res**)))
  (define tbl* (combine res**))
  (with-output-to-file "data/success-tbl.rktd" #:exists 'replace (lambda () (pretty-write tbl*)))
  (print-table (first tbl*))
  (newline)
  (let* ((xx* (second tbl*))
         (title* (car xx*))
         (row** (group-by second (cdr xx*))))
    (for ((row* (in-list row**)))
      (print-table (cons title* row*))
      (newline)))
  (void))

(define (take-some x*)
  (values x*)
  #;(take x* 6))

(define (go-bm bm*)
  ;; forall benchmark, split configs into: immediate, hopeless, feasible
  ;; forall strategy + mode, forall feasible: 
  (define bm-name (trailfile-bb (car bm*)))
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
  (define (fast-enough? cfg perf)
    (good-overhead? perf))
  (define-values [immediate# other#]
    (for/fold ((imm# (hash))
               (oth# (hash))
               #:result (values imm# oth#))
              (((cfg perf) (in-hash perf#)))
      (if (fast-enough? cfg perf)
        (values (hash-set imm# cfg #true) oth#)
        (values imm# (hash-set oth# cfg #true)))))
  (define-values [feasible# hopeless#]
    (for/fold ((good# (hash))
               (bad# (hash))
               #:result (values good# bad#))
              ((cfg (in-hash-keys other#)))
      (if (reachable? cfg immediate#)
        (values (hash-set good# cfg #true) bad#)
        (values good# (hash-set bad# cfg #true)))))
  (seascape num-configs immediate# feasible# hopeless#))

(define (reachable? cfg tgt#)
  (for/or ((tgt (in-hash-keys tgt#)))
    (config-reachable? cfg tgt)))

(define (bm-trails bm* perf# ss)
  (define win# (seascape-immediate# ss))
  (define hope# (seascape-feasible# ss))
  (define num-feasible (hash-count hope#))
  (cons
    (let* ((num-toggle
             (for/sum (((cfg perf) (in-hash perf#))
                       #:unless (hash-ref win# cfg #f))
               (if (or (hash-ref win# (config->deep cfg) #f)
                       (hash-ref win# (config->shallow cfg) #f))
                 1 0))))
      (trailres "toggle" "-" num-toggle "-" "-" "-" "-" num-feasible))
    (for/list ((td (in-list bm*)))
      (define ss (trailfile-ss td))
      (define mm (trailfile-mm td))
      (define-values [mono-win win-1 win-2 win-3 win-N]
        (for/fold ((num-mono 0)
                   (num-1 0)
                   (num-2 0)
                   (num-3 0)
                   (num-N 0)
                   #:result (values num-mono num-1 num-2 num-3 num-N))
                  ((rr (in-list (file->value (trailfile-ff td))))
                   #:unless (hash-ref win# (row-cfg rr) #f))
          (define cc* (row-cc* rr))
          (define win? (and (not (null? cc*)) (hash-ref win# (last cc*) #f)))
          (define dips
            ;; BEWARE subtle: dips refers to previous best, not prev.
            (and win?
                 (let loop ((prev-best (hash-ref perf# (row-cfg rr)))
                            (num-dip 0)
                            (cc cc*))
                   (if (null? cc)
                     num-dip
                     (let ((next (hash-ref perf# (car cc))))
                       (if (overhead<=? next prev-best)
                         (loop next num-dip (cdr cc))
                         (loop prev-best (+ 1 num-dip) (cdr cc))))))))
          (values (+ (if (and dips (< dips 1)) 1 0) num-mono)
                  (+ (if (and dips (< dips 2)) 1 0) num-1)
                  (+ (if (and dips (< dips 3)) 1 0) num-2)
                  (+ (if (and dips (< dips 4)) 1 0) num-3)
                  (+ (if win? 1 0) num-N))))
      (trailres ss
            mm
            mono-win
            win-1
            win-2
            win-3
            win-N
            num-feasible))))

(define (combine res**)
  (list
    (combine-seascape res**)
    (combine-trails res**)))

(define (print-table x*)
  (displayln
    (simple-table->string
      #:align '(left right)
      x*)))

(define (combine-seascape res**)
  (cons
    (list "benchmark" "%immediate" "%hopeless" "%feasible" "3^N")
    (for/list ((res (in-list res**)))
      (define bb (bmres-bb res))
      (define ss (bmres-seascape res))
      (define nc (seascape-num-configs ss))
      (define nw (hash-count (seascape-immediate# ss)))
      (define ny (hash-count (seascape-feasible# ss)))
      (define nn (hash-count (seascape-hopeless# ss)))
      (list bb (pctstr2 nw nc) (pctstr2 nn nc) (pctstr2 ny nc) nc))))

(define (combine-trails res**)
  (cons
    (list "strategy" "mode" "%mono-win" "%1-win" "%2-win" "%3-win" "%N-win" "total scenarios")
    (cons
      (trail-collect res** "toggle" "-")
      (for*/list ((strat (in-list (all-strategy-name*)))
                  (mode (in-list (all-mode-name*))))
        (trail-collect res** strat mode)))))

(define (trail-collect res** strat mode)
      (define-values [num-mono num-1 num-2 num-3 num-N num-total]
        (for*/fold ((nm 0)
                   (n1 0)
                   (n2 0)
                   (n3 0)
                   (nN 0)
                   (nt 0)
                   #:result (values nm n1 n2 n3 nN nt))
                  ((rr (in-list res**))
                   (tt (in-list (bmres-trails rr)))
                   #:when (and (equal? (trailres-ss tt) strat)
                               (equal? (trailres-mm tt) mode)))
          (values (+ nm (trailres-win-0 tt))
                  (+ n1 (->real (trailres-win-1 tt)))
                  (+ n2 (->real (trailres-win-2 tt)))
                  (+ n3 (->real (trailres-win-3 tt)))
                  (+ nN (->real (trailres-win-N tt)))
                  (+ nt (trailres-num-feasible tt)))))
      (list strat
            mode
            (pctstr2 num-mono num-total)
            (pctstr2 num-1 num-total)
            (pctstr2 num-2 num-total)
            (pctstr2 num-3 num-total)
            (pctstr2 num-N num-total)
            num-total))

(define (->real n)
  (if (real? n) n 0))

(module+ main (go))

