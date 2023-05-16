#lang racket

(require
  text-table
  "base.rkt"
  "runtime.rkt")

;; - - -

(define (rnd n) (~r n #:precision '(= 1)))

(define (tough-pairs-improved bm-name)
  ;; A <==> B, fast together, slow separate
  (define fsm# (benchmark->perf# bm-name))
  (define nmod (log3 (hash-count fsm#)))
  (printf "===~n~a ~s mod~n" bm-name nmod)
  (define tough-pair*
    (for*/list ((ii (in-range nmod))
                (jj (in-range (+ ii 1) nmod))
                (p0 (in-value (hash-ref fsm# (make-config nmod #:D ii))))
                (p1 (in-value (hash-ref fsm# (make-config nmod #:D jj))))
                (p2 (in-value (hash-ref fsm# (make-config nmod #:D (list ii jj)))))
                #:when (and (overhead>? p0 p2)
                            (overhead>? p1 p2)
                            (overhead<=? p2 (list 10 0))))
      (list ii jj p0 p1 p2)))
  #;(printf "  tough pair~n")
  #;(pretty-write tough-pair*)
  (define diff*
    (apply append
      (for/list ((pp (in-list tough-pair*)))
        (define ii (first pp))
        (define jj (second pp))
        (define p0 (third pp))
        (define p1 (fourth pp))
        (define s0 (hash-ref fsm# (make-config nmod #:S ii)))
        (define s1 (hash-ref fsm# (make-config nmod #:S jj)))
        (append
          (if (< (first s0) (first p0))
            (list (list ii (first p0) (first s0)))
            '())
          (if (< (first s1) (first p1))
            (list (list jj (first p1) (first s1)))
            '())))))
  (printf "  improved~n")
  (pretty-write
    (map
      (lambda (x)
        (format "(~a :: ~a ==> ~a)"
                (make-config nmod #:D (first x))
                (rnd (second x))
                (rnd (third x))))
      (sort
        diff*
        >
        #:key (lambda (x) (- (second x) (third x))))))
  (newline)
  (void))

(define (where-slow bm-name)
  (define perf# (benchmark->perf# bm-name))
  (define slow# (make-hash))
  (void
    (for (((k v) (in-hash perf#))
          #:when (and (deep-config? k)
                      (<= 10 (first v))))
      (hash-add1! slow# (config->num-types k))))
  (printf "~a  ~a over 10x~n" bm-name (apply + (hash-values slow#)))
  (pretty-write (sort (hash->list slow#) < #:key car))
  (void))

(define (low-levels bm-name)
  ;; how often does toggling help on low lattice levels?
  (define perf# (benchmark->perf# bm-name))
  (define nmod (log3 (hash-count perf#)))
  (define-values [l1-good l1-total] (try-toggle one-typed perf#))
  (define-values [l2-good l2-total] (try-toggle two-typed perf#))
  (define-values [l3-good l3-total]
                 (if (< 2 nmod) (try-toggle (has-typed 3) perf#) (values "" "")))
  (define-values [l4-good l4-total]
                 (if (< 3 nmod) (try-toggle (has-typed 4) perf#) (values "" "")))
  (define-values [lnn-good lnn-total]
                 (if (< 4 nmod) (try-toggle (has-typed (- nmod 2)) perf#) (values "" "")))
  (define-values [ln-good ln-total]
                 (if (< 5 nmod) (try-toggle (has-typed (- nmod 1)) perf#) (values "" "")))
  (list nmod
        l1-good (pctstr2 l1-good l1-total)
        l2-good (pctstr2 l2-good l2-total)
        l3-good (if (real? l3-good) (pctstr2 l3-good l3-total) "")
        l4-good (if (real? l4-good) (pctstr2 l4-good l4-total) "")
        lnn-good (if (real? lnn-good) (pctstr2 lnn-good lnn-total) "")
        ln-good (if (real? ln-good) (pctstr2 ln-good ln-total) "")
        ))

(define ((has-typed n) cfg)
  (= n (num-typed-modules cfg)))

(define (one-typed cfg)
  (= 1 (num-typed-modules cfg)))

(define (two-typed cfg)
  (= 2 (num-typed-modules cfg)))

(define (num-typed-modules cfg)
  (for/sum ((cc (in-string cfg))
            #:unless (eq? #\0 cc))
    1))

(define (try-toggle p? perf#)
  (for/fold ((num-fast 0)
             (num-total 0))
            (((k v) (in-hash perf#))
             #:when (p? k))
    (values (+ (if (and
                     (not (good-overhead? v))
                     (good-overhead? (hash-ref perf# (config->shallow k))))
                   1 0) num-fast)
            (+ 1 num-total))))

(define go low-levels)

(module+ main
  ; (go 'fsm)
  ; (go 'fsmoo)
  ; (go 'kcfa)
  ; (go 'jpeg)
  ; (go 'tetris)
  #;(for ((bm (in-list '(forth fsm fsmoo kcfa jpeg suffixtree snake take5 zombie
                       tetris dungeon acquire synth))))
   (go bm))
  (print-simple-table
    #:align '(left right)
    (cons
      (list "benchmark" "N" "L1-S" "%" "L2-S" "%" "L3-S" "%" "L4-S" "%" "L(n-2)-S" "%" "L(n-1)-S" "%")
      (for/list ((bm (in-list (all-benchmark-name*))))
        (cons bm (go bm)))))
  (void))


