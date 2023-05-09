#lang racket

(require
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

(define go where-slow)

(module+ main
  ; (go 'fsm)
  ; (go 'fsmoo)
  ; (go 'kcfa)
  ; (go 'jpeg)
  ; (go 'tetris)
  (for ((bm (in-list '(forth fsm fsmoo kcfa jpeg suffixtree snake take5 zombie
                       tetris dungeon acquire synth))))
   (go bm))
  (void))


