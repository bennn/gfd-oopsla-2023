#lang racket

(require
  "base.rkt"
  ppict/2
  pict pict-abbrevs
  (only-in gtp-pict make-lattice))

(define out-kind
  'pdf
  #;'png)

(define text-size 11)

(define fsm-info
  '#hash((configs . 81)
         (under-1x . 18)
         (over-1x . 63)
         (premean . 158390/32823)
         (premedian .  7514/2605)
         (postmean .  104123/23445)
         (postmedian . 7464/2605)))

(define fsm-step0-lattice
'(("0000" 1 1 1)
 ("0001" 2 783/2605 1311/2605)
 ("0010" 2 3977/2605 7548/2605)
 ("0100" 2 7388/2605 7464/2605)
 ("1000" 2 2664/2605 3742/521)
 ("0011" 4 453/521 6213/2605)
 ("0101" 4 5688/2605 5761/2605)
 ("0110" 4 3061/2605 8926/2605)
 ("1001" 4 922/2605 17446/2605)
 ("1010" 4 3861/2605 4725/521)
 ("1100" 4 7482/2605 23613/2605)
 ("0111" 8 1249/2605 7183/2605)
 ("1011" 8 2094/2605 22553/2605)
 ("1101" 8 5898/2605 22384/2605)
 ("1110" 8 2544/2605 4892/521)
 ("1111" 16 861/2605 4737/521)))

(define fsm-step1-lattice
'(("0000" 0 #f #f)
 ("0001" 0 #f #f)
 ("0010" 2 3977/2605 7548/2605)
 ("0100" 2 7388/2605 7464/2605)
 ("1000" 2 2664/2605 3742/521)
 ("0011" 0 #f #f)
 ("0101" 4 5688/2605 5761/2605)
 ("0110" 4 3061/2605 8926/2605)
 ("1001" 0 #f #f)
 ("1010" 4 3861/2605 4725/521)
 ("1100" 4 7482/2605 23613/2605)
 ("0111" 0 #f #f)
 ("1011" 6 5681/2605 22553/2605)
 ("1101" 8 5898/2605 22384/2605)
 ("1110" 0 #f #f)
 ("1111" 0 #f #f)))

(define (bblur pp)
  (cellophane pp 0.1))

(define (go bm-name info lat0 lat1)
  (printf "\\bmname{~a} basics:~n\\begin{tabular}{llll}~a configurations (cfgs) & ~a fast & ~ax avg. slowdown & ~a median slowdown\\end{tabular}~n"
          bm-name (hash-ref info 'configs) (hash-ref info 'under-1x)
          (int-round (hash-ref info 'premean)) (int-round (hash-ref info 'premedian)))
  (printf "\\bmname{~a} post ~a mean ~a median~n"
          bm-name (int-round (hash-ref info 'postmean)) (int-round (hash-ref info 'postmedian)))
  #;(save-pict* 'fsm-lattice0 (mklattice lat0))
  #;(save-pict* 'fsm-lattice1 (mklattice lat1))
  (save-pict* 'fsm-lattice (combine-lat  (mklattice lat0) (mklattice lat1)))
  (void))

(define (combine-lat pp0 pp1)
  (ht-append
    9
    pp0
    (vc-append (blank 0 10) (colorize (arrow 38 0) "dim gray"))
    pp1))

(define int-round exact-round)

(define (mklattice ll)
  (define num-bits (string-length (caar ll)))
  (make-lattice num-bits (mk-node ll) #:y-margin 8))

(define (bool->str bb*)
  (apply string (for/list ((bb (in-list bb*))) (if bb #\1 #\0))))

(define ((mk-node ll) bb*)
  (define key (bool->str bb*))
  (define val (assoc key ll))
  (define nn (bool-node bb*))
  (define-values [top txt]
    (cond
      [(fast-cfg? val)
       (values (ppict-do (bblur nn) #:go (coord 1/2 1/2 'cc) (make-check-pict (* 2 (pict-height nn))))
               " \\ ")]
      [else
        (values nn
                (format "~a cfgs\\~a"
                        (second val)
                        (intervalstr (int-round (third val)) (int-round (fourth val)))))]))
  (vc-append 4 top (lbltext txt)))

(define (intervalstr lo hi)
  (format "~ax to ~ax" (if (zero? lo) "<1" lo) hi))

(define (fast-cfg? val)
  (not (third val)))

(define (bool-node bb*)
  (apply hc-append (map bool->square bb*)))

(define (bool->square bb)
  (filled-square 10 (if bb "black" "white")))

(define (filled-square ww color)
  (filled-rounded-rectangle ww ww 1 #:draw-border? #t #:border-color "black" #:color color))

(define (lbltext str)
  (apply vc-append 2 (map lbltext-one (string-split str "\\"))))

(define (lbltext-one str)
  (text str 'roman text-size))

(define (save-pict* sym pp)
  (save-pict (format "data/~a.~a" sym out-kind)
             (add-rectangle-background
               pp
               #:radius 0
               #:color "white"
               #:draw-border? #f
               #:x-margin 2
               #:y-margin 2)))

(module+ main
  (go 'fsm fsm-info fsm-step0-lattice fsm-step1-lattice))


