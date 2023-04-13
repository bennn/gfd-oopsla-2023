#lang racket

(require
  "base.rkt"
  "success.rkt"
  math/statistics
  racket/runtime-path
  text-table
  pict pict-abbrevs
  plot/no-gui
  (except-in plot/utils min* max*))

;; Color picker: https://davidmathlogic.com/colorblind
;; Wong: https://www.nature.com/articles/nmeth.1618

(define wong*
  (map hex-triplet->color%
       '( #x000000
          #xe69f00 ; orange
        ;  #x56b4e9 ; lite blue
        ;  #xf0e442 ; yellow
          #x0072b2 ; red
          #xd55e00 ; blu
          #xcc79a7 ; pink
          )))

(define wong-lite*
  (map hex-triplet->color%
       '( #xffffff
          #xffcf50 ; orange
        ;  #x56b4e9 ; lite blue
        ;  #xfff452 ;yellow
          #x2092d2 ;red
          #xf57e20 ;blu
          #xec99c7 ;pink
          )))

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
  (("randomB") "random boundary")
  #;(("randomD") "random optimistic")
  #;(("randomS") "random conservative")
  (else (raise-argument-error 'bg-strategy->cd-strategy  "unknown" str))))

(define cd-strategy*
  '(
    "optimistic"
    "cost-aware optimistic"
    "conservative"
    "cost-aware conservative"
    "configuration-aware"
    "random boundary"
    ;;"random optimistic"
    ;;"random conservative"
    "toggling"))

(define (shortname str)
  (string-replace
    (string-replace
      (string-replace
        (string-replace
          (string-replace
            str
            "configuration" "config.")
          "optimistic" "opt.")
        "conservative" "con.")
      "random boundary" "random")
    "toggling" "tog."))

(define (agnostic-strategy? str)
  (member str (take-right cd-strategy* 2)))

(define (con-strategy? str)
  (string-contains? str "conservative"))

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
                                cadr)
                      (apply append vv)))
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

(define (t:blackhole)
  (define tbl (car (success-tbl)))
  (define title* (list "Benchmark" "$3^N$" "\\% Hopeless"))
  (define row*
    (for/list ((rr (in-list (cdr tbl))))
      (list (bmname (first rr))
            (number->string (fifth rr))
            (hide-zeropct (third rr)))))
  (tex-table (cons title* row*))
  (void))

(define (hide-zeropct str)
  (if (equal? str "0.00%")
    "---"
    str))

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

(define (f:strategy-overall #:hope? [hope? #f])
  ;; TODO `hope?` is wrong, computed over all configs instead of all hopeful,
  ;; gotta redo successrktd
  (define tbl (cadr (success-tbl)))
  (define total-scenarios
    (list-ref (cadr tbl) (if hope? 8 9)))
  (printf "~a total ~a scenarios~n" (if hope? "(hopeful)" "") total-scenarios)
  (define rect**
    (cd-sort
     (filter-not null?
      (for/list ((rr* (in-list (group-by first (cdr tbl)))))
       (filter first
        (for/list ((rr (in-list rr*)))
          (list (bg-strategy->cd-strategy (first rr))
                (second rr)
                (pct2 (third rr) total-scenarios)
                (pct2 (fourth rr) total-scenarios)
                (pct2 (fifth rr) total-scenarios)
                (pct2 (sixth rr) total-scenarios)
                (pct2 (seventh rr) total-scenarios)
                (pct2 (eighth rr) total-scenarios))))))))
  (define-values [rect-agnostic** -other-rect**]
    (partition (compose1 agnostic-strategy? caar) rect**))
  (define other-rect**
    (if hope?
      (filter-not (compose1 con-strategy? caar) -other-rect**)
      -other-rect**))
  (define x-offset 1/2)
  (define num-mode 3)
  (define num-ag (length rect-agnostic**))
  (define num-other (length other-rect**))
  (define pp
    (parameterize ((plot-y-ticks (pct-ticks))
                   (plot-x-far-ticks no-ticks)
                   (plot-x-ticks (label-ticks
                                   ;; TODO magic numbers
                                   (append
                                     (for/list ((rr (in-list other-rect**))
                                                (ii (in-naturals)))
                                       (list (+ 2 (* ii 4)) (caar rr)))
                                     (for/list ((rr (in-list rect-agnostic**))
                                                (ii (in-naturals)))
                                       (list (+ (* 4 num-other) 1
                                                ii (* ii 1/2)) (caar rr)))))))
      (plot-pict
        (list
          (for/list ((y (in-range 9)))
            (hrule (* 10 (+ y 1))
                   #:width 1
                   #:color "black"
                   #:alpha 0.06))
          (for/list ((mode-num (in-range num-mode)))
            (for/list ((rect* (in-list other-rect**))
                       (strat-num (in-naturals)))
              (rrect (+ mode-num
                        (* (+ num-mode 1) strat-num))
                     (cddr (list-ref rect* mode-num))
                     #:color (+ 1 mode-num)
                     #:x0 x-offset)))
          (for/list ((rect* (in-list rect-agnostic**))
                     (ii (in-naturals)))
            (rrect (+ ii (* 1/2 ii))
                   (cddr (aggregate rect*))
                   #:color (+ num-mode 1)
                   #:x0 (+ x-offset (* 4 (length other-rect**))))))
        #:x-label #f
        #:y-label #f
        #:x-min 0
        #:x-max (+ (* 4 num-other) num-ag 1 1/2)
        #:y-min 0
        #:y-max 100
        #:width (if hope? 500 600)
        #:height 300)))
  (define out-name (format "strategy-overall~a.~a" (if hope? "-hope" "") (*out-kind*)))
  (printf "save-pict ~a~n" out-name)
  (save-pict
    (build-path data-dir out-name)
    (hc-append
      2
      pp
      (legend-pict num-mode)))
  (void))

(define (vector-add1 v k)
  (vector-set! v k (+ 1 (vector-ref v k))))

(define (h2hstr->sm from-mode)
  (let* ((str* (string-split from-mode "_")))
    (values (bg-strategy->cd-strategy (car str*))
            (string-join (cdr str*) "_"))))

(define (h2h-key str)
  (or
    (index-of cd-strategy* str)
    (index-of (append (all-mode-name*) (list "")) (string-replace str "profile" "prf"))
    (raise-argument-error 'h2h-key "key" str)))

(define (f:head2head #:all? [all? #f])
  (define num-sm (length all-sm-name*))
  (define fave*
    (if all? (drop-right all-sm-name* 4) '("opt_boundary")))
  (define vv (file->value (build-path data-dir "h2h.rktd")))
  (define pp*
      (for/list ((from-mode (in-list fave*)))
        (define from-key (sm->idx from-mode))
        (define-values [strategy mode]
          (h2hstr->sm from-mode))
        (define win* (make-vector num-sm 0))
        (define tie* (make-vector num-sm 0))
        (define los* (make-vector num-sm 0))
        (define num-configs 0)
        (void
          (for* ((bm (in-list vv))
                 (res (in-hash-values (cadr bm))))
            (set! num-configs (add1 num-configs))
            (define from-win? (eq? #\1 (string-ref res from-key)))
            (for ((to-mode (in-list all-sm-name*))
                  (to-key (in-naturals)))
              (define to-win? (eq? #\1 (string-ref res to-key)))
              (cond
                ((eq? from-win? to-win?)
                 (vector-add1 tie* to-key))
                ((and from-win?  (not to-win?))
                 (vector-add1 win* to-key))
                ((and to-win?  (not from-win?))
                 (vector-add1 los* to-key))
                (else
                  (error 'h2hwtf))))))
        (define x-order*
          (let* ((to* (for/list ((to-mode (in-list all-sm-name*))
                                 (to-key (in-naturals))
                                 #:unless (string-contains? to-mode "randomB_prf"))
                        (list to-mode to-key)))
                 (fkey (lambda (str)
                         (define-values [strategy mode] (h2hstr->sm str))
                         (list (if (string-contains? strategy "random")
                                 (length to*)
                                 (if (string-contains? strategy "togg")
                                   (add1 (length to*))
                                   (h2h-key mode)))
                               (h2h-key strategy)))))
            (sort to* << #:key (compose1 fkey car) #:cache-keys? #true)))
        (define num-gap (length (all-mode-name*)))
        (define pp
          (parameterize ((plot-y-ticks (pct-ticks))
                         #;(TODO plot-x-ticks (exact-ticks num-sm))
                         (plot-x-far-ticks no-ticks))
            (plot-pict
              (for/list ((vec (in-list (list #;tie* los* win*)))
                         (cc (in-naturals #;0 1)))
                (rectangles
                 (filter values
                  (for/list ((ni (in-list x-order*))
                             (xx (in-naturals)))
                    (define smode (car ni))
                    (define ii (cadr ni))
                    (define yy
                      (if (string-contains? smode "randomB")
                        (mean (list (vector-ref vec ii)
                                    (vector-ref vec (+ ii 1))
                                    (vector-ref vec (+ ii 2))))
                        (vector-ref vec ii)))
                    (define x0 (+ (- xx 1/4)
                                  (cond
                                    ((string-contains? smode "prf_t") 1)
                                    ((string-contains? smode "prf_s") 2)
                                    ((string-contains? smode "random") 3)
                                    ((string-contains? smode "togg") 3)
                                    (else 0))
                                  (* 1/4 (sub1 cc))))
                    (and (< 0 yy)
                      (vector (ivl x0 (+ x0 1/4))
                              (ivl 0 (pct2 yy num-configs))))))
                  #:line-width 1
                  #:line-color cc
                  #:color cc
                  #:alpha 0.8))
              #:width 600
              #:height 300
              #:x-min -1/2
              #:x-max (+ 1/2 (sub1 (length x-order*)) num-gap)
              #:y-min 0
              #:y-max 100
              #:x-label #f
              #:y-label #f
              #:title #f)))
        (define title-pict (lbltxt (format "~a ~a, ~a configs" strategy mode num-configs)))
        (ht-append
          4
          (vl-append title-pict pp)
          (cheap-h2h-legend x-order*))))
  (save-pict
    (build-path data-dir (format "head-to-head.~a" (*out-kind*)))
    (cond
      ((null? (cdr pp*))
       (car pp*))
      (all?
       (ptable
         #:ncols 3
         #:row-sep 4
         #:col-sep 4
         pp*))
      (else
       (apply vl-append 4 pp*))))
  (void))

(define (app:head2head)
  (define num-sm (length all-sm-name*))
  (define fave*
    ;; '("opt_boundary" "cost-opt_boundary" "limit-con_boundary")
    (drop-right all-sm-name* 4))
  (define vv (file->value (build-path data-dir "h2h.rktd")))
  (define out-dir (build-path data-dir "h2h"))
  (void (ensure-dir out-dir))
  (for ((tgt-bm (in-list (map car vv))))
    (define pp*
      (parameterize ((plot-y-ticks (pct-ticks))
                     #;(plot-x-ticks (exact-ticks num-sm))
                     (plot-x-far-ticks no-ticks))
        (for/list ((from-mode (in-list fave*)))
          (define from-key (sm->idx from-mode))
          (define-values [strategy mode]
            (let* ((str* (string-split from-mode "_")))
              (values (bg-strategy->cd-strategy (car str*))
                      (string-join (cdr str*) "_"))))
          (define win* (make-vector num-sm 0))
          (define tie* (make-vector num-sm 0))
          (define los* (make-vector num-sm 0))
          (define num-configs 0)
          (void
            (for* ((bm (in-list vv))
                   #:when (equal? (car bm) tgt-bm)
                   (res (in-hash-values (cadr bm))))
              (set! num-configs (add1 num-configs))
              (define from-win? (eq? #\1 (string-ref res from-key)))
              (for ((to-mode (in-list all-sm-name*))
                    (to-key (in-naturals)))
                (define to-win? (eq? #\1 (string-ref res to-key)))
                (cond
                  ((eq? from-win? to-win?)
                   (vector-add1 tie* to-key))
                  ((and from-win?  (not to-win?))
                   (vector-add1 win* to-key))
                  ((and to-win?  (not from-win?))
                   (vector-add1 los* to-key))
                  (else
                    (error 'h2hwtf))))))
          (define pp
            (plot-pict
              (for/list ((vec (in-list (list tie* los* win*)))
                         (cc (in-naturals)))
                (rectangles
                  (for/list ((yy (in-vector vec))
                             (xx (in-naturals))
                             #:when (< 0 yy))
                    (define x0 (+ (- xx 1/4) (* 1/4 cc)))
                    (vector (ivl x0 (+ x0 1/4))
                            (ivl 0 (pct2 yy num-configs))))
                  #:line-width 1
                  #:line-color cc
                  #:color cc
                  #:alpha 0.8))
              #:width 600
              #:height 300
              #:y-min 0
              #:y-max 100
              #:x-label #f
              #:y-label #f
              #:title #f))
          (define title-pict (lbltxt (format "~a ~a ~a, ~a configs" tgt-bm strategy mode num-configs)))
          (ht-append
            4
            (vl-append title-pict pp)
            (cheap-h2h-legend)))))
    (define out-name (format "~a.~a" tgt-bm (*out-kind*)))
    (printf "save-pict ~a~n" out-name)
    (save-pict
      (build-path out-dir out-name)
      (ptable
        #:ncols 3
        #:row-sep 4
        #:col-sep 4
        pp*)
      #;(apply vl-append 4 pp*))
    (void))
  (void))

(define (cheap-h2h-legend [x-order* #f])
  (apply
    vl-append
    (for/list ((ni (in-list (or x-order*
                                (for/list ((str (in-list all-sm-name*))
                                           (ii (in-naturals)))
                                   (list str ii))))))
      (lbltxt (format "~a. ~a" (second ni) (first ni))))))

(define (aggregate rect*)
  (if (null? (cdr rect*))
    (car rect*)
    (let* ((strat (caar rect*))
           (mode "-")
           (num* (apply map list (map cddr rect*)))
           (avg* (map mean num*))
           (std* (map stddev/mean avg* num*)))
      #;(log-error "aggregate: ~a~n stddevs: ~a nums: ~a" strat std* num*)
      (log-error "aggregate: avg of ~a runs, max stddev ~a" (length (car num*)) (~r (apply max std*) #:precision '(= 2)))
      (list*
        strat
        mode
        avg*))))

(define swatch-w 12/100)

(define (legend-pict num-mode)
  (define ymax 6)
  (define x-txt (- 1 2/10))
  (define rrect-y* (map add1 (range ymax)))
  (define (lbltxt2 str)
    (lbltxt str #:size+ 2))
  ;; colors = 1 ... num-mode+1
  (parameterize ((plot-y-ticks no-ticks)
                 (plot-y-far-ticks no-ticks)
                 (plot-x-ticks no-ticks)
                 (plot-x-far-ticks no-ticks))
    (plot-pict
      (list
        ;; tower shapes
        (rrect 0 rrect-y* #:color 0 #:w 60/100)
        (for/list ((y (in-list rrect-y*))
                   (str (in-list '("strict success" "1-loose" "2-loose" "3-loose" "N-loose" "improved"))))
          (lblpoint
            (vector x-txt (- y 1/2))
            (lbltxt2 str)))
        ;; mode colors
        (for/list ((ii (in-range (add1 num-mode)))
                   (str (in-list '("feature-specific" "statistical (total)" "statistical (self)" "agnostic"))))
          (define yy (* (+ 1 (if (= ii num-mode) 5/4 3/4) ii) -1))
          (list
            (rswatch (+ 10/100 (* (+ 3/100 swatch-w) (if (= ii num-mode) 1 ii)))
                     yy #:color (+ 1 ii))
            (lblpoint
              (vector x-txt (+ yy 1/4))
              (lbltxt2 str)))))
      #:x-min 0
      #:x-max 175/100
      #:y-min (+ ymax 1/2)
      #:y-max (- ymax)
      #:x-label #f
      #:y-label #f
      #:title #f
      #:width 140
      #:height 200)))

(define (rswatch x y #:color cc)
  (rectangles
    (list (vector (ivl x (+ x swatch-w))
                  (ivl y (+ y 1/2))))
    #:line-width 1
    #:line-color (my->pen-color cc)
    #:color (my->brush-color cc)
    #:alpha 0.9))

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

(define (lbltxt str #:size+ [size+ 0])
  (text str 'roman (+ size+ 9)))

(define (rrect x y* #:color c #:x0 [x0 0] #:w [w% 1])
  (define x-mid (+ x0 x 1/2))
  (define old-y 0)
  (define L-1 (sub1 (length y*)))
  (define L-2 (sub1 L-1))
  (filter values
    (for/list ((y (in-list y*))
               (ii (in-naturals))
               #:when (>= y old-y))
      (define y0 old-y)
      (set! old-y y)
      (define alpha
        (if (= L-2 ii)
          0.5
          (- 0.9 (/ ii 10))))
      (define x-gap
        (if (= L-1 ii)
          0.01
          (if (= L-2 ii)
            0.12
            (* 1/2 alpha))))
      (define rstyle
        (if (= L-2 ii)
          'fdiagonal-hatch
          'solid))
      (rectangles (list
                    (vector
                      (ivl (* w% (- x-mid x-gap)) (* w% (+ x-mid x-gap)))
                      (ivl y0 y)))
                  #:line-color (my->pen-color c)
                  #:style rstyle
                  #:color ((if (= L-2 ii) my->pen-color my->brush-color) c)
                  #:alpha alpha))))

(define (my->pen-color c)
  (list-ref wong* c))

(define (my->brush-color c)
  #;(color%-update-alpha (my->pen-color c) 0.2)
  (list-ref wong-lite* c))

(define (label-ticks rl*)
  (define (my-layout ax-min ax-max)
    (for/list ((rl (in-list rl*)))
      (pre-tick (car rl) #true)))
  (define (my-format ax-min ax-max pt*)
    (for/list ((pt (in-list pt*)))
      (shortname (cadr (assoc (pre-tick-value pt) rl*)))))
  (ticks my-layout my-format))

(define (pct-ticks)
  (define (my-layout ax-min ax-max)
    (append
      (list (pre-tick 0 #true))
      (for/list ((ii (in-range 1 (+ 1 (exact-floor (/ ax-max 10))))))
        (pre-tick (* 10 ii) (= ii 5)))
      (list (pre-tick (exact-floor ax-max) #true))))
  (ticks my-layout pct-format))

(define (mini-pct-ticks)
  (define (my-layout ax-min ax-max)
    (append
      (list (pre-tick 0 #true))
      (for/list ((ii (in-range 1 (+ 1 ax-max))))
        (pre-tick ii (= ii 5)))
      (list (pre-tick ax-max #true))))
  (ticks my-layout pct-format))

(define (pct-format ax-min ax-max pt*)
  (for/list ((pt (in-list pt*))
             (ii (in-naturals)))
    (if (pre-tick-major? pt)
      (format "~a%" (pre-tick-value pt))
      (make-string ii #\NUL))))

(define (lblpoint xy pp)
  (point-pict
    xy pp
    #:anchor 'left
    #:point-size 0
    #:point-sym 'none))

(define (go)
  (parameterize ( (*out-kind* 'png))
    #;(t:baseline-trouble)
    #;(f:strategy-overall)
    #;(f:strategy-overall #:hope? #true)
    #;(app:strategy-overall)
    #;(f:deathplot)
    (f:head2head)
    #;(t:blackhole)
    #;(app:head2head)
    (void)))

(module+ main
  (go))

