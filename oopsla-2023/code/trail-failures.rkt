#lang racket

(require
  "base.rkt"
  text-table
  file/glob
  pict pict-abbrevs
  plot/no-gui
  (except-in plot/utils min* max*))

(define out-kind 'pdf)

(define (t:count-failures)
  (define bm* (values #;take-some (all-benchmark-name*)))
  (define out-file (format "data/trail-failures.rktd"))
  (define data**
    (for/list ((bm (in-list bm*)))
      (list bm (glob (build-path "t1-img" (format "~a-*rktd" bm))))))
  (define tbl
    (for*/list ((bm+data* (in-list data**))
                (data-file (in-list (cadr bm+data*))))
      (define bm-name (car bm+data*))
      (define row* (file->value data-file))
      (define-values [_bm ss mm] (split-filename (file-name-from-path data-file)))
      (define num-configs (length row*)) ;; TODO filter easy ones
      (define zp* (zero-progress row* 10))
      (unless (or (string-contains? ss "random")
                  (string-contains? ss "toggle")
                  (null? zp*))
        (printf "~a ~a zero progress failures~n ~e~n"
                (list bm-name ss mm) (length zp*) zp*))
      (list
        bm-name
        ss
        mm
        (num-fail-configs row*)
        num-configs)))
  #;(displayln
    (simple-table->string tbl))
  (void))

(define (zero-progress row* too-slow)
  (define ovr (row*->get-overhead row*))
  (for/list ((rr (in-list row*))
             #:when (and (< too-slow (ovr (row-ms rr)))
                         (equal? (row-cfg rr) (last (row-ss rr)))))
    (row-cfg rr)))

(define (num-fail-configs row*)
  (define ovr (row*->get-overhead row*))
  (for/sum ((rr (in-list row*))
            #:when (row-trail-failure? rr ovr))
    1))

(define (go)
  (t:count-failures)
  (void))

(module+ main (go))

