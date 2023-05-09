#lang racket

(require
  "base.rkt"
  "success.rkt"
  file/glob)

;; ---

(define (ignore-strategy? str)
  (or (string-contains? str "randomD")
      (string-contains? str "randomS")
      (string-contains? str "limit-opt")))

(define (ignore-benchmark? str)
  (string=? str "sieve"))

(define (why-error fn*)
  (define hhh
    (for/fold ((acc (hash)))
              ((fn (in-list fn*)))
      (define filename (file-name-from-path fn))
      (define-values [bm ss pp] (split-filename filename))
      (define strategy (format "~a-~a" ss pp))
      (printf "process ~a~n" (path->string filename))
      (if (ignore-strategy? strategy)
        acc
        (for/fold ((acc acc))
                  ((rr (in-list (file->value fn))))
          (define end (row-end rr))
          (define frac-typed
            (let* ((trail (row-cc* rr))
                   (final-cfg (if (null? trail) (row-cfg rr) (last trail)))
                   (nc (string-length final-cfg))
                   (nt (for/sum ((cc (in-string final-cfg)) #:unless (eq? #\0 cc)) 1)))
              (/ nt nc)))
          (if (string-contains? (~a end) "error")
            (hash-update acc
                         end
                         (lambda (x)
                           (list
                             (+ (first x) (if (< frac-typed 1/2) 1 0))
                             (+ (second x) (if (> frac-typed 1/2) 1 0))
                             (hash-add1 (third x) frac-typed)
                             (hash-add1 (fourth x) strategy)))
                         (lambda () (list 0 0 (hash) (hash))))
            acc)))))
  (pretty-write
    hhh
    ;(sort
    ;  (map
    ;    (lambda (x) (list (car x) (sort (hash->list (cdr x)) string<? #:key car)))
    ;    (hash->list hhh))
    ;  >
    ;  #:key (lambda (x) (apply + (map cdr (cadr x)))))
  )
  (void))

(define (no-internal fn*)
  (define successres (file->value "data/success-res.rktd"))
  (define hhh
    (for/fold ((acc (hash)))
              ((fn (in-list fn*)))
      (define filename (file-name-from-path fn))
      (define-values [bm ss pp] (split-filename filename))
      (define strategy (format "~a-~a" ss pp))
      (if (or (ignore-benchmark? bm)
              (ignore-strategy? strategy))
        acc
        (let ()
          (printf "go ~s~n" (path->string filename))
          (define hopeless# (seascape-hopeless#
                              (for/first ((bmr (in-list successres))
                                          #:when (equal? bm (bmres-bb bmr)))
                                (bmres-seascape bmr))))
          ;; TODO split prf_total / self
          (define prf* (map file->cc# (glob (build-path trail-dir (format "~a-~a-prf_*.rktd" bm ss)))))
          (for/fold ((acc acc))
                    ((rr (in-list (file->value fn)))
                     (ii (in-naturals))
                     #:when (eq? (cadr (row-end rr)) 'no-internal-mods))
            (define curr-cfg (row-cfg rr))
            #;(when (or (< ii 10) (zero? (modulo ii 1000)))
              (printf " cfg ~a = ~a~n" ii curr-cfg))
            (cond
              ((hash-has-key? hopeless# curr-cfg)
               (hash-add1 acc 'hopeless))
              (else
                (define profile-next*
                  (for/list ((prf (in-list prf*)))
                    (or (hash-ref prf curr-cfg #f)
                        (error 'notfound-weird))))
                (define profile-stuck? (andmap null? profile-next*))
                ;; TODO split total / self / neither
                (if profile-stuck?
                  acc
                  (hash-add1 acc 'prf_ok)))))))))
  (pretty-write hhh)
  (void))

(define (file->cc# prf)
  (for/hash ((rr2 (in-list (file->value prf))))
    (values (row-cfg rr2) (row-cc* rr2))))

(define (take-some xx)
  (values xx)
  #;(take xx 3))


;; ---

(define (go-overview)
  (define dd* (take-some (glob (build-path trail-dir "*.rktd"))))
  (why-error dd*))

(define (go-no-internal)
  (define dd* (take-some (glob (build-path trail-dir "*boundary.rktd"))))
  (no-internal dd*))

(define (go)
  #;(go-overview)
  (go-no-internal)
  (void))

(module+ main (go))



