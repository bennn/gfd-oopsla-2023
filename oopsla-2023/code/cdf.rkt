#lang racket ;;/base

;; CDF = cumulative distribution function
;; * x0 = overhead
;; * x1 = end steps
;; * ...?

(require
  "base.rkt"
  file/glob
  pict pict-abbrevs
  plot/no-gui
  (except-in plot/utils min* max*))

;; ===

(define out-kind 'pdf)

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

(struct cdfline [strat mode bucket*] #:transparent)

(define (plot-cdf bm line* #:vrule [draw-vrule? #f] #:title [my-title #f])
  (parameterize ()
    (define-values [global-xmax global-ymax]
      (for/fold ((gx 0) (gy 0) #:result (values (add1 gx) (add1 gy)))
                ((nn (in-list line*)))
        (define b* (cdfline-bucket* nn))
        (values (max gx (if (null? b*) 1 (apply max (map first b*))))
                (max gy (if (null? b*) 1 (apply max (map second b*)))))))
    (plot-pict
     (append
      (list
       (carcdr-shuffle
        (for/list ((nn (in-list line*))
                   (ii (in-naturals)))
          (define bucket* (cdfline-bucket* nn))
          ;; (define local-xmin (apply min (map first bucket*)))
          (define local-ymax (if (null? bucket*) 1 (apply max (map second bucket*))))
          (define strategy (cdfline-strat nn))
          (pointlines
            ;; -interval `(#(,local-xmin 0) #(,local-xmax 0)) #:line1-color 0 #:line2-color cc
            (append bucket* (list (vector global-xmax local-ymax)))
            #:label (format "~a ~a" strategy (cdfline-mode nn))
            #:alpha 0.5
            #:sym (list-ref point-sym* ii)
            #:color (strat->color strategy)))))
      (if draw-vrule?
       (list (vrule 1 #:color "orange" #:width 5 #:alpha 0.2))
       '()))
      #:title (or my-title (format "~a" bm))
      #:legend-anchor 'outside-right-top
      #:x-label #f
      #:y-label #f
      #:y-min 0
      #:y-max global-ymax
      #:x-min 1
      #:x-max global-xmax
      #:width 400
      #:height 200)))

(define (carcdr-shuffle xy*)
  (append (map car xy*)
          (map cdr xy*)))

(define (pointlines data #:label lbl #:alpha alpha #:color color #:sym sym)
  (cons
    (lines data
           #:label #f
           #:alpha alpha
           #:color color)
    (points data
            #:label lbl
            #:alpha alpha
            #:color color
            #:sym sym)))

(define strat->color
  (let* ((nn* (all-strategy-name*)))
    (lambda (str) (add1 (index-of nn* str)))))

(define (cdfize bucket*)
  (let loop ((acc 0)
             (bb bucket*))
    (cond
      ((null? bb)
       '())
      (else
        (define hd (car bb))
        (define tl (cdr bb))
        (define xx (+ acc (second hd)))
        (cons (list (first hd) xx)
              (loop xx tl))))))

(define (make-plotter cdf-kind f-bucket)
  (define bm* (values #;take-some (all-benchmark-name*)))
  (define name (format "cdf-~a" cdf-kind))
  (define data**
    (for/list ((bm (in-list bm*)))
      (define all* (glob (build-path "t1-img" (format "~a-*rktd" bm))))
      (filter-not
        (lambda (pp)
          (define str (path->string pp))
          (or (string-contains? str "toggle")
              (string-contains? str "random")))
        all*)))
  (for ((pmode (in-list (values #;take-some (all-mode-name*)))))
    (define pict*
      (for/list ((bm (in-list bm*))
                 (-dd* (in-list data**)))
  (printf "~a ~a ~a~n" cdf-kind pmode bm)
        (define dd* (filter (lambda (pp) (string-contains? (path->string pp) pmode)) -dd*))
        (define *num-fail (box (list #f #f)))
        (define line*
          (for/list ((pp (in-list dd*))) 
            (define-values [_bm strat mode] (split-filename (file-name-from-path pp)))
            (define all-row* (file->value pp))
            (set-box! *num-fail
                      (let* ((nn (length (filter-interesting all-row*)))
                             (ff (unbox *num-fail))
                             (omin (car ff))
                             (omax (cadr ff)))
                        (list (if (or (not omin) (< nn omin)) nn omin)
                              (if (or (not omax) (< omax nn)) nn omax))))
            (define bucket* (cdfize (f-bucket all-row*)))
 #;(when (and (eq? cdf-kind 'overhead) (equal? (~a _bm) "snake"))
   (printf "snake ~a ~a~n" strat mode)
   (pretty-write bucket*))
            (cdfline strat mode bucket*)))
        (define-values [min-fail max-fail] (apply values (unbox *num-fail)))
        (plot-cdf bm line*
                  #:title (format "~a, ~a fails"
                                  bm
                                  (if (= min-fail max-fail)
                                    min-fail
                                    (format "~a to ~a" min-fail max-fail)))
                  #:vrule #f #;(if (eq? 'overhead cdf-kind) #true #f))))
    (save-pict
      (format "data/~a_~a.~a" name pmode out-kind)
      (ptable
        #:ncols 3
        #:row-sep 8
        #:col-sep 10
        (cons (blank) pict*)))))

(define (filter-interesting rr*)
  ;; TODO remove trivial-success configs
  (filter row-trail-failure? rr*))

(define (f:cdf-overhead)
  (make-plotter 'overhead bucket/overhead))

(define (f:cdf-steps)
  (make-plotter 'steps bucket/steps))

(define (bucket/overhead row*)
  (define utime (row-ms (car row*)))
  (define (f-overhead rr) (row-overhead rr utime))
  (fold-row* row* f-overhead))

(define (bucket/steps row*)
  (fold-row* row* (compose1 sub1 row-ns)))

(define (fold-row* row* ff)
  (define b#
    (for/fold ((acc (hash)))
              ((rr (in-list (filter-interesting row*))))
      (hash-add1 acc (ff rr))))
  (buckets->list b#))

(define (row-overhead rr uu)
  (/ (exact-round (* 100 (/ (row-ms rr) uu))) 100))

(define (buckets->list b#)
  (let* ((vv (hash->list b#))
         (vv (sort vv < #:key car))
         (vv (map (lambda (xy) (list (car xy) (cdr xy))) vv)))
    vv))

(define (go)
  (f:cdf-overhead)
  (f:cdf-steps))

(module+ main (go))

