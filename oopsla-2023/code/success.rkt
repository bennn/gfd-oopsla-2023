#lang racket

(require
  "base.rkt"
  "runtime.rkt"
  file/glob
  text-table
  racket/runtime-path)

;; ---

(define-runtime-path trail-dir "../t1-img/")

(define-syntax-rule (with-output-to-file+ fn arg* ...)
  (begin (with-output-to-file fn arg* ...)
         (printf "save file ~s~n" fn)))

(struct row (cfg end cc*) #:prefab)
;; TODO delete

(struct trailfile (bb ss mm ff) #:prefab)
(struct trailres (ss mm win-0 win-1 win-2 win-3 win-N better-N num-feasible) #:prefab)
(struct bmres (bb seascape trails) #:prefab)
(struct seascape (num-configs immediate# feasible# hopeless#) #:prefab)

(define (all-trail-data)
  (for/list ((fn (in-glob (build-path trail-dir "*rktd")))
             #:unless (let ((str (path->string (file-name-from-path fn))))
                        (regexp-match? #rx"random(D|S)" str)))
    (define-values [bm strategy mode] (split-filename (file-name-from-path fn)))
    (trailfile bm strategy mode fn)))

(define (go)
  (go-success)
  (void))

(define (go-success)
  (define data* (filter (lambda (x)
                          #true
                          #;(string-contains? (trailfile-bb x) "tetris"))
                        (all-trail-data)))
  (define bm** (take-some
                 (sort
                   (filter-not null? (group-by trailfile-bb data*))
                   < #:key (compose1 benchmark-index trailfile-bb car))))
  (define res**
    #;(file->value "data/success-res.rktd")
    (map go-bm bm**))
  (with-output-to-file+ "data/success-res.rktd" #:exists 'replace (lambda () (pretty-write res**)))
  #;(let ()
    (define xy** (map go-xy bm**))
    (with-output-to-file "data/success-xy.rktd" #:exists 'replace (lambda () (pretty-write (map (lambda (x) (map car x)) xy**))))
    (for* ((bmxy (in-list xy**))
           (rr (in-list bmxy)))
      (define bm (first (car rr)))
      (define ss (second (car rr)))
      (define mm (third (car rr)))
      (with-output-to-file (format "data/success-xy-~a-~a-~a.rktd" bm ss mm)
                           #:exists 'replace (lambda () (pretty-write (cadr rr))))))
  (define tbl* (combine res**))
  (with-output-to-file+ "data/success-tbl.rktd" #:exists 'replace (lambda () (pretty-write tbl*)))
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
  (printf "go-bm: ~a~n" bm-name)
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
  (define hopeless# (seascape-hopeless# ss))
  (define num-scenario (+ (hash-count hope#) (hash-count hopeless#)))
  (cons
    (let ()
      (define-values [num-win num-better]
        (for/fold ((nw 0)
                   (nb 0)
                   #:result (values nw nb))
                  (((cfg perf) (in-hash perf#))
                   #:unless (hash-ref win# cfg #f))
          (define deep-cfg (config->deep cfg))
          (define shallow-cfg (config->shallow cfg))
          (define deep-win? (hash-ref win# deep-cfg #f))
          (define shallow-win? (hash-ref win# shallow-cfg #f))
          (values
            (+ nw (if (or deep-win? shallow-win?) 1 0))
            (+ nb (if (or deep-win? shallow-win?)
                    1
                    (if (or (overhead>? perf (hash-ref perf# deep-cfg))
                            (overhead>? perf (hash-ref perf# shallow-cfg)))
                      1 0))))))
      (trailres "toggle" "-" num-win "-" "-" "-" "-" num-better num-scenario))
    (for/list ((td (in-list bm*)))
      (define ss (trailfile-ss td))
      (define mm (trailfile-mm td))
      (define-values [mono-win win-1 win-2 win-3 win-N better-N]
        (for/fold ((num-mono 0)
                   (num-1 0)
                   (num-2 0)
                   (num-3 0)
                   (num-N 0)
                   (num-better 0)
                   #:result (values num-mono num-1 num-2 num-3 num-N num-better))
                  ((rr (in-list (file->value (trailfile-ff td))))
                   #:unless (hash-ref win# (row-cfg rr) #f))
          (define cc* (row-cc* rr))
          (define win? (and (not (null? cc*)) (ormap (compose1 good-overhead? (lambda (x) (hash-ref perf# x))) cc*)))
          (define better? (and (not (null? cc*))
                               (overhead>? (hash-ref perf# (row-cfg rr))
                                           (hash-ref perf# (last cc*)))))
          (define dips
            ;; BEWARE subtle: dips refers to previous best, not prev.
            (and win?
                 (let loop ((prev-best (hash-ref perf# (row-cfg rr)))
                            (num-dip 0)
                            (cc cc*))
                   (if (null? cc)
                     num-dip
                     (let ((next (hash-ref perf# (car cc))))
                       (cond
                         ((good-overhead? next)
                          num-dip)
                         ((overhead<=? next prev-best)
                          (loop next num-dip (cdr cc)))
                         (else
                          (loop prev-best (+ 1 num-dip) (cdr cc)))))))))
          (values (+ (if (and dips (< dips 1)) 1 0) num-mono)
                  (+ (if (and dips (< dips 2)) 1 0) num-1)
                  (+ (if (and dips (< dips 3)) 1 0) num-2)
                  (+ (if (and dips (< dips 4)) 1 0) num-3)
                  (+ (if win? 1 0) num-N)
                  (+ (if better? 1 0) num-better))))
      (trailres ss
            mm
            mono-win
            win-1
            win-2
            win-3
            win-N
            better-N
            num-scenario))))

(define (combine res**)
  (list
    (combine-seascape res**)
    (combine-trails res**)))

(define (print-table x*)
  (displayln
    (simple-table->string
      #:align '(left right)
      x*)))

(define (go-xy bm*)
  (define bm-name (trailfile-bb (car bm*)))
  (printf "go-xy: ~a~n" bm-name)
  (define perf# (benchmark->perf# bm-name))
  (define win# (seascape-immediate# (bm-seascape perf#)))
  (define point*
    (for/list ((td (in-list bm*)))
      (define ss (trailfile-ss td))
      (define mm (trailfile-mm td))
      (define xy*
        (for/list ((rr (in-list (file->value (trailfile-ff td))))
                   #:unless (hash-ref win# (row-cfg rr) #f))
          (define start-cfg (row-cfg rr))
          (define start-perf (hash-ref perf# start-cfg))
          (define end-cfg
            (let loop ((prev-best start-perf)
                       (cc* (row-cc* rr)))
              (cond
                ((null? cc*)
                 start-cfg)
                ((null? (cdr cc*))
                 (car cc*))
                (else
                 (define cfg (car cc*))
                 (define next (hash-ref perf# cfg))
                 (if (or (good-overhead? next)
                         (not (overhead<=? next prev-best)))
                   cfg
                   (loop next (cdr cc*)))))))
          (define end-perf (hash-ref perf# end-cfg))
          ;; BEWARE: ignore stddev
          (list (car start-perf) (car end-perf))))
      (list (list bm-name ss mm) xy*)))
  point*)


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
    (list "strategy" "mode" "%mono-win" "%1-win" "%2-win" "%3-win" "%N-win" "%improved" "total scenarios")
    (cons
      (trail-collect res** "toggle" "-")
      (for*/list ((strat (in-list (all-strategy-name*)))
                  (mode (in-list (all-mode-name*))))
        (trail-collect res** strat mode)))))

(define (trail-collect res** strat mode)
      (define-values [num-mono num-1 num-2 num-3 num-N num-better num-total]
        (for*/fold ((nm 0)
                   (n1 0)
                   (n2 0)
                   (n3 0)
                   (nN 0)
                   (nB 0)
                   (nt 0)
                   #:result (values nm n1 n2 n3 nN nB nt))
                  ((rr (in-list res**))
                   (tt (in-list (bmres-trails rr)))
                   #:when (and (equal? (trailres-ss tt) strat)
                               (equal? (trailres-mm tt) mode)))
          (values (+ nm (trailres-win-0 tt))
                  (+ n1 (->real (trailres-win-1 tt)))
                  (+ n2 (->real (trailres-win-2 tt)))
                  (+ n3 (->real (trailres-win-3 tt)))
                  (+ nN (->real (trailres-win-N tt)))
                  (+ nB (->real (trailres-better-N tt)))
                  (+ nt (trailres-num-feasible tt)))))
      (when (zero? num-total)
        (raise-arguments-error 'trail-collect
                               "uhoh zero total configs"
                               "strat" strat
                               "mode" mode))
      (list strat
            mode
            (pctstr2 num-mono num-total)
            (pctstr2 num-1 num-total)
            (pctstr2 num-2 num-total)
            (pctstr2 num-3 num-total)
            (pctstr2 num-N num-total)
            (pctstr2 num-better num-total)
            num-total))

(define (->real n)
  (if (real? n) n 0))

(module+ main (go))

