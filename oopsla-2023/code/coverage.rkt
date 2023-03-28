#lang racket

;; coverage = how many lattice points do the strategies succeed on?
;;  goal = find patterns where strategy/mode A fails but strategy/mode B succeeds
;;
;; 1. forall benchmarks, all strategies, collect successful configs
;;    (easy from data files)
;; 2. boundary ; profile_total ; profile_self
;;    try in order, what new configs does each level add?
;;    save file with the configs
;;    print table of counts, for each benchmark, each strategy
;;    TODO may need to reorder, self;total
;; 3. for strategies in order, what new configs added?
;;    opt ; cost-opt
;;    opt ; limit-con
;;    opt ; con
;;    opt ; cost-con
;;    opt ; limit-opt
;; 4. TODO study the 2,3 configs, any patterns?

(require
  "base.rkt"
  file/glob
  text-table
  racket/pretty
  pict pict-abbrevs
  plot/no-gui
  (except-in plot/utils min* max*))

;; ---

(define mode-dir "t1-mode-coverage")
(define strat-dir "t1-strategy-coverage")

(define (t:mode-coverage mode*)
  (define bm* (all-benchmark-name*))
  (define data**
    (for/list ((bm (in-list bm*)))
      (printf "... collect ~s~n" bm)
      (list bm (benchmark->mode-coverage bm mode*))))
  (ensure-dir mode-dir)
  (tex-table data** mode*)
  (void))

(define (benchmark->mode-coverage bm mode*)
  (filter values
    (for/list ((fn (in-glob (build-path "t1-img" (format "~a-*rktd" bm)))))
      (define-values [bm strat mode] (split-filename (file-name-from-path fn)))
      (and
        (member mode mode*)
        (not (string-contains? strat "random"))
        (not (string-contains? strat "toggle"))
        (list (list strat mode)
              (for/list ((row (in-list (file->value fn)))
                         #:when (eq? 'success (car (row-end row))))
                (row-cfg row)))))))

(define (benchmark->strat-coverage bm ss*)
  ;; TODO combine with above, change filters
  (filter values
    (for/list ((fn (in-glob (build-path "t1-img" (format "~a-*rktd" bm)))))
      (define-values [bm strat mode] (split-filename (file-name-from-path fn)))
      (and
        (member strat ss*)
        (string=? mode "boundary")
        (list (list strat mode)
              (for/list ((row (in-list (file->value fn)))
                         #:when (eq? 'success (car (row-end row))))
                (row-cfg row)))))))

(define (pct aa bb)
  (exact-round (* 100 (/ aa bb))))

(define (tex-table data** mode*)
  (define all-data
      (cons
        (list* "benchmark" "strategy" mode*)
        (apply append
          (for/list ((bm+x (in-list data**)))
            (define bm-name (car bm+x))
            (define num-cfgs (bm->num-configs bm-name))
            (define strat** (group-by caar (cadr bm+x)))
            (for/list ((strat* (in-list strat**)))
              (define seen# (make-hash))
              (define s-name (caar (car strat*)))
              (list* bm-name
                     s-name
                     (for/list ((mm (in-list mode*)))
                         (for/first ((row (in-list strat*))
                                     #:when (string=? mm (cadar row)))
                           (define success (cadr row))
                           (define newe
                             (for/list ((cfg (in-list success))
                                       #:unless (hash-has-key? seen# cfg))
                               (hash-set! seen# cfg #true)
                               cfg))
 (with-output-to-file
  #:exists 'replace
  (build-path mode-dir (format "~a-~a-~a.rktd" bm-name s-name mm))
  (lambda ()
   (for-each displayln newe)))
                           (define nn (length newe))
                           (format "~a (~a%)" nn (pct nn num-cfgs))))))))))
  (displayln
    (simple-table->string
      #:align '(left left right)
      all-data
      )))

(define (tex-table-strat data** ss*)
  (displayln
    (simple-table->string
      (cons
        (list* "benchmark" "mode" ss*)
        (apply append
          (for/list ((bm+x (in-list data**)))
            (define bm-name (car bm+x))
            (define num-cfgs (bm->num-configs bm-name))
            (define strat** (list (cadr bm+x)))
            (for/list ((strat* (in-list strat**)))
              (define seen# (make-hash))
              (list* bm-name
                     "boundary"
                     (for/list ((ss (in-list ss*)))
                         (for/first ((row (in-list strat*))
                                     #:when (string=? ss (caar row)))
                           (define success (cadr row))
                           (define newe
                             (for/list ((cfg (in-list success))
                                       #:unless (hash-has-key? seen# cfg))
                               (hash-set! seen# cfg #true)
                               cfg))
 (with-output-to-file
  #:exists 'replace
  (build-path strat-dir (format "~a-~a-~a.rktd" bm-name ss "boundary"))
  (lambda ()
   (for-each displayln newe)))
                           (define nn (length newe))
                           (format "~a (~a%)" nn (pct nn num-cfgs))))))))))))

(define (t:strategy-coverage strat*)
  (define bm* (all-benchmark-name*))
  (define data**
    (for/list ((bm (in-list bm*)))
      (printf "... s collect ~s~n" bm)
      (list bm (benchmark->strat-coverage bm strat*))))
  (ensure-dir strat-dir)
  (tex-table-strat data** strat*)
  (void))

(define (go)
  #;(t:mode-coverage (all-mode-name*))
  (t:strategy-coverage '("opt" "cost-opt" "limit-opt"))
  (void))

(module+ main
  (go))

