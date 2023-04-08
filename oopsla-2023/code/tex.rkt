#lang racket

(require
  "base.rkt"
  "success.rkt"
  racket/runtime-path
  text-table
  pict pict-abbrevs
  plot/no-gui
  (except-in plot/utils min* max*))

(define-runtime-path data-dir "../data")
(define *out-kind* (make-parameter 'pdf))

(define (mode->idx str)
  (case str
    (("boundary") 0)
    (("prf_total") 1)
    (("prf_self") 2)
    (else (raise-argument-error 'mode->idx "mode?" str))))

(define (bg-strategy->cd-strategy str)
  (case str
  (("toggle") "toggling")
  (("opt") "optimistic")
  (("cost-opt") "cost-aware optimistic")
  (("limit-con") "configuration-aware")
  (("con") "conservative")
  (("cost-con") "cost-aware conservative")
  (("limit-opt") #f)
  (("randomD") "random optimistic")
  (("randomS") "random conservative")
  (else (raise-argument-error 'bg-strategy->cd-strategy  "unknown" str))))

(define cd-strategy*
  '("optimistic"
    "cost-aware optimistic"
    "conservative"
    "cost-aware conservative"
    "configuration-aware"
    "random optimistic"
    "random conservative"
    "toggling"))

(define (agnostic-strategy? str)
  (member str (take-right cd-strategy* 3)))

(define (cd-strategy-index str)
  (or (index-of cd-strategy* str)
      (raise-argument-error 'cd-strategy-index "cd?" str)))

(define (tex-table x* #:align [align #f])
  (print-simple-table
    #:align (or align '(left right))
    #:border-style 'latex
    x*))

(define (success-tbl)
  (file->value (build-path data-dir "success-tbl.rktd")))

(define (points-by-strategy vv)
  (let* ((all (filter (compose1 (lambda (x) (define vv (bg-strategy->cd-strategy x))
                                            (and vv (not (agnostic-strategy? vv))))
                                cadr) (apply append vv)))
         (ss* (group-by cadr all))
         (ss* (sort ss* < #:key (compose1 cd-strategy-index bg-strategy->cd-strategy cadar) #:cache-keys? #true)))
    ss*))

(define (t:baseline-trouble)
  (define tbl (car (success-tbl)))
  (define title* (list "Benchmark" "$3^N$" "\\% Slow"))
  (define row*
    (for/list ((rr (in-list (cdr tbl))))
      (list (bmname (first rr))
            (number->string (fifth rr))
            (rev-pct (second rr)))))
  (tex-table (cons title* row*))
  (void))

(define (rev-pct str)
  (define nn (pct->number str))
  (format "~a\\%"
          (~r #:precision '(= 2)
              (/ (- 10000 (* 100 nn)) 100))))

(define (pct->number str)
  (string->number (substring str 0 (sub1 (string-length str)))))

(define (bmname str)
  (format "\\bmname{~a}" str))

(define (cd-sort row**)
  (sort row** < #:key (compose1 cd-strategy-index caar)))

(define (f:strategy-overall)
  ;; TODO stacked bar, 1 2 3
  (define tbl (cadr (success-tbl)))
  (define total-scenarios (last (cadr tbl)))
  (define rect**
    (cd-sort
     (filter-not null?
      (for/list ((rr* (in-list (group-by first (cdr tbl)))))
       (filter first
        (for/list ((rr (in-list rr*)))
          (list (bg-strategy->cd-strategy (first rr))
                (second rr)
                (pct->number (third rr)))))))))
  #;(void
    (for* ((rect* (in-list rect**))
           (rect (in-list rect*)))
      (printf "~a ~a~n" (first rect) (second rect))))
  (define-values [rect-agnostic** other-rect**]
    (partition (compose1 agnostic-strategy? caar) rect**))
  (define x-offset 1/2)
  (define num-mode 3)
  (parameterize ((plot-y-ticks (pct-ticks)))
    (plot-file
      (list
        (for/list ((mode-num (in-range num-mode)))
          (for/list ((rect* (in-list other-rect**))
                     (strat-num (in-naturals)))
            (rrect (+ mode-num
                      (* (+ num-mode 1) strat-num))
                   (third (list-ref rect* mode-num))
                   #:color (+ 1 mode-num)
                   #:x0 x-offset)))
        (for/list ((rect* (in-list rect-agnostic**))
                   (ii (in-naturals)))
          (rrect ii
                 (third (car rect*))
                 #:color (+ num-mode 1 ii)
                 #:x0 (+ 1 (* 2 x-offset) (* 4 (length other-rect**))))))
      (build-path data-dir (format "strategy-overall.~a" (*out-kind*)))
      #:x-min 0
      ;; #:x-max (+ (* 2 x-offset) (* 2 (length rect**)))
      #:y-min 0
      #:y-max 100
      #:width 600
      #:height 350))
  (void))

(define (group-by-mode x*)
  (sort (group-by third x*) < #:key (compose1 mode->idx third car)))

(define (take-some x*)
  #;(values x*)
  (take x* 6))

(define (f:deathplot)
  ;; TODO (low priority, appendix) rerun for N-dip ... clearer difference?
  (define ww 100)
  (define overhead-max 50)
  (define pp*
    (for/list ((ss* (in-list (points-by-strategy (file->value (build-path data-dir "success-xy.rktd"))))))
      (define ss
        (let* ((hdr (car ss*))) (second hdr)))
      (cons
        (lbltxt ss)
        (for/list ((gg* (in-list (group-by-mode ss*))))
          (define mm
            (let* ((hdr (car gg*))) (third hdr)))
          (define ii (index-of (all-mode-name*) mm))
          (define ps 'dot #;(list-ref point-sym* ii))
          (plot-pict
            (list
              (function values #:width 1 #:color "purple")
              (for/list ((bm (in-list (map car gg*))))
                (define fname (format "success-xy-~a-~a-~a.rktd" bm ss mm))
                (printf "deathplot ~s~n" fname)
                (points
                  (for/list ((xy (in-list (file->value (build-path data-dir fname)))))
                    (define xx (car xy))
                    (define yy (cadr xy))
                    (vector xx yy))
                  #:color ii
                  #:alpha 0.1
                  #:size 2
                  #:line-width 1
                  #:sym ps)))
            #:title #f
            #:x-label #f
            #:y-label #f
            #:x-min 0
            #:x-max overhead-max
            #:y-min 0
            #:y-max overhead-max
            #:width ww
            #:height ww)))))
  (define pp
    (ptable
      #:ncols 4
      #:row-sep 2
      #:col-align cc-superimpose
      #:col-sep 6
      (append pp* (cons (blank) (map lbltxt (all-mode-name*))))))
  (save-pict (format "data/deathplot.~a" (*out-kind*)) pp)
  (void))

(define (lbltxt str)
  (text str 'roman 9))

(define (rrect x y #:color c #:x0 x0)
  (rectangles (list
                (vector
                  (ivl (+ x0 x) (+ x0 x 1))
                  (ivl 0 y)))
              #:line-color (->pen-color c)
              #:color (->brush-color c)
              #:alpha 0.8))

(define (pct-ticks)
  (define (my-layout ax-min ax-max)
    (append
      (list (pre-tick 0 #true))
      (for/list ((ii (in-range 1 (+ 1 (exact-floor (/ ax-max 10))))))
        (pre-tick (* 10 ii) #f))
      (list (pre-tick (exact-floor ax-max) #true))))
  (define (my-format ax-min ax-max pt*)
    (for/list ((pt (in-list pt*))
               (ii (in-naturals)))
      (if (pre-tick-major? pt)
        (format "~a%" (pre-tick-value pt))
        (make-string ii #\NUL))))
  (ticks my-layout my-format))

(define (go)
  (parameterize ( #;(*out-kind* 'png))
    #;(t:baseline-trouble)
    (f:strategy-overall)
    #;(f:deathplot)
    (void)))

(module+ main
  (go))

