#lang at-exp slideshow

;; 18 min slot
;; 15 min talk 3 min q/a
;; 14:18 ? Thurs October 26 2023
;; ... convert to 4:3 format

;; [X] https://docs.google.com/presentation/d/1vBZkcBu-4AHWRd1AMt6b8OuokaHR91juc6ahOCvOgMU/edit#slide=id.g28099dacf64_0_64
;; [X] simple draft
;; [X] practice talk thursday 11am GOLLY
;; [X] nice draft

(require
  "../code/color.rkt"
  (only-in "lightbulb.rkt" lightbulb)
  racket/class
  racket/draw
  racket/format
  racket/match
  racket/list
  racket/string
  racket/runtime-path
  pict
  ppict/2
  pict-abbrevs
  gtp-pict
  (only-in images/icons/misc close-icon magnifying-glass-icon)
  (only-in images/icons/symbol check-icon)
  (only-in images/icons/control stop-icon)
  images/icons/style
  ppict/pict ppict/tag
  pict-abbrevs/slideshow
  plot/no-gui (except-in plot/utils min* max*))

(define-runtime-path img-dir "./src")
(define src-dir img-dir)

(define turn revolution)

(define x%->pixels w%->pixels)
(define y%->pixels h%->pixels)

(define pico-x-sep (w%->pixels 1/100))
(define tiny-x-sep (w%->pixels 2/100))
(define border-x-sep (w%->pixels 4/100))
(define small-x-sep (w%->pixels 5/100))
(define smol-x-sep small-x-sep)
(define med-x-sep (w%->pixels 10/100))
(define big-x-sep (w%->pixels 15/100))
(define medd-x-sep med-x-sep)
(define bigg-x-sep big-x-sep)

(define pico-y-sep (h%->pixels 1/100))
(define tiny-y-sep (h%->pixels 2/100))
(define small-y-sep (h%->pixels 5/100))
(define smol-y-sep small-y-sep)
(define med-y-sep (h%->pixels 10/100))
(define big-y-sep (h%->pixels 15/100))
(define medd-y-sep med-y-sep)
(define bigg-y-sep big-y-sep)

(define slide-top 4/100)
(define slide-left 2/100)
(define slide-right (- 1 slide-left))
(define slide-bottom 82/100)
(define slide-text-left (* 3 slide-left)) ;; 3/2 SD 4:3
(define head-left 20/100) ;; slide-left SD 4:3
(define head-right (- 1 head-left)) ;; slide-right SD 4:3
(define text-left slide-text-left)
(define slide-text-right (- 1 slide-text-left))
(define text-right slide-text-right)
(define slide-heading-top (* 1.4 slide-top))
(define slide-text-top (* 4 slide-top))
(define hi-text (* 6 slide-top))
(define lo-text (* 2.5 hi-text))
(define slide-text-bottom slide-bottom)
(define lesson-x 18/100)

(define slide-text-coord (coord slide-text-left slide-text-top 'lt))
(define slide-text-coord-left slide-text-coord)
(define slide-text-coord-mid (coord 1/2 slide-text-top 'ct))
(define slide-text-coord-right (coord slide-text-right slide-text-top 'rt))
(define slide-text-coord-l  slide-text-coord-left)
(define slide-text-coord-m   slide-text-coord-mid)
(define slide-text-coord-r slide-text-coord-right)
(define heading-text-coord (coord head-left slide-heading-top 'lt))
(define heading-text-coord-left heading-text-coord)
(define heading-text-coord-mid (coord 1/2 slide-heading-top 'ct))
(define heading-text-coord-right (coord head-right slide-heading-top 'rt))
(define heading-coord heading-text-coord)
(define heading-coord-left heading-text-coord-left)
(define heading-coord-mid heading-text-coord-mid)
(define heading-coord-right heading-text-coord-right)
(define heading-coord-l  heading-coord-left)
(define heading-coord-m  heading-coord-mid)
(define heading-coord-r  heading-coord-right)
(define bottom-coord-left (coord slide-left slide-text-bottom 'lb))
(define bottom-coord-mid (coord 1/2 slide-text-bottom 'cb))
(define bottom-coord-right (coord slide-right slide-text-bottom 'rb))
(define bottom-coord-l bottom-coord-left)
(define bottom-coord-m bottom-coord-mid)
(define bottom-coord-r bottom-coord-right)
(define center-coord (coord 1/2 1/2 'cc))
(define title-coord (coord 1/2 26/100 'ct))
(define hi-text-coord-left (coord slide-text-left hi-text 'lt))
(define hi-text-coord-mid (coord 1/2 hi-text 'ct))
(define hi-text-coord-right (coord slide-text-right hi-text 'rt))
(define hi-text-coord-l hi-text-coord-left)
(define hi-text-coord-m   hi-text-coord-mid)
(define hi-text-coord-r hi-text-coord-right)
(define hi-text-coord-ll  (coord 48/100 hi-text 'rt))
(define hi-text-coord-rr (coord 52/100 hi-text 'lt))
(define lo-text-coord-left (coord slide-text-left lo-text 'lt))
(define lo-text-coord-mid (coord 1/2 lo-text 'ct))
(define lo-text-coord-right (coord slide-text-right lo-text 'rt))
(define all-lang-coord (coord 99/100 1/2 'rc))
(define lesson-coord-h (coord lesson-x hi-text  'lt))
(define lesson-coord-m (coord lesson-x (+ 15/100 hi-text) 'lt))
(define lesson-coord-l (coord lesson-x (+ 30/100 hi-text) 'lt))
(define title-coord-m (coord 1/2 23/100 'ct))

(define default-line-width 4)
(define default-arrow-size 14)
(define large-arrow-size 18)

(define code-brush-alpha 0.6)

(define (color%++ c n)
  (make-object color%
               (byte-round (+ (send c red) n))
               (byte-round (+ (send c green) n))
               (byte-round (+ (send c blue) n))
               (send c alpha)))

(define (byte-round n)
  (if (< n 0)
    0
    (if (< 255 n)
      255 n)))

(define at-sign @"@")

(define black (hex-triplet->color% #x222222))
(define gray (string->color% "light gray"))
(define white (string->color% "white"))
(define lite-grey (hex-triplet->color% #xeeeeee)) ; "gainsboro"
(define transparent (color%-update-alpha white 0))
(define dark-orange (hex-triplet->color% #xE05626))
(define lite-orange (hex-triplet->color% #xF89C3F))
(define dark-blue (hex-triplet->color% #x002E6D))
(define bg-dark-blue (hex-triplet->color% #x2C6B91))
(define bg-lite-blue (hex-triplet->color% #x357C9F))
(define lite-blue (hex-triplet->color% #xC0EFFF))
(define lite-green (hex-triplet->color% #x00b18f))

(define utah-red (hex-triplet->color% #xCC0000))
(define utah-black (hex-triplet->color% #x000000))
(define utah-white (hex-triplet->color% #xFFFFFF))
(define utah-sunrise (hex-triplet->color% #xFFB81D))
(define utah-lake (hex-triplet->color% #x3ABFC0))
(define utah-crimson (hex-triplet->color% #x890000))
(define utah-granite (hex-triplet->color% #x708E99))
(define utah-darkgrey (hex-triplet->color% #xE2E6E6))
(define utah-litegrey (hex-triplet->color% #xF7F9FB))

(define typed-color utah-sunrise)
(define untyped-color utah-granite)
(define shallow-color utah-lake)
(define concrete-color utah-crimson)
(define primitive-color utah-lake)
(define deep-color typed-color)
(define typed-brush-color (color%++ typed-color 20))
(define shallow-pen-color shallow-color #;(hex-triplet->color% #xffc20a) )
(define deep-pen-color deep-color #;(hex-triplet->color% #x0c7bdc))
(define concrete-pen-color concrete-color)
(define primitive-pen-color primitive-color)
(define untyped-pen-color untyped-color)
(define shallow-brush-color (color%-update-alpha shallow-pen-color 0.4) #;lite-orange #;(hex-triplet->color% #xfdc008))
(define deep-brush-color (color%-update-alpha deep-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define concrete-brush-color (color%-update-alpha concrete-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define primitive-brush-color (color%-update-alpha primitive-pen-color 0.4) #;(hex-triplet->color% #x0a79da))
(define untyped-brush-color (color%-update-alpha untyped-pen-color 0.4) #;(color%++ untyped-color 20))
(define fog-3k1 (hex-triplet->color% #xDBCAC2))
(define neutral-brush-color fog-3k1)
(define green0-3k1 (hex-triplet->color% #x71BE8D))
(define green1-3k1 (hex-triplet->color% #x598F61))
(define green2-3k1 (hex-triplet->color% #x4F7459))
(define red0-3k1 (hex-triplet->color% #xF0749C))
(define red1-3k1 (hex-triplet->color% #xC3476F))
(define apple-green lite-green)
(define apple-red red1-3k1)
(define typed-pen-color #f)
(define validate-pen-color red1-3k1)
(define validate-brush-color (color%-update-alpha validate-pen-color code-brush-alpha))
(define happy-cloud-color lite-blue)
(define sad-cloud-color dark-blue)
(define default-line-color dark-blue)
(define browncs-frame-color dark-blue)
(define hilite-frame-color dark-orange)
(define blame-color typed-color)
(define shallow-bg-color (color%-update-alpha shallow-pen-color 0.2))
(define deep-bg-color  (color%-update-alpha deep-pen-color 0.2))
(define typed-bg-color deep-bg-color)
(define untyped-bg-color (color%-update-alpha untyped-pen-color 0.2))

(define emph-color (hex-triplet->color% #x304E59))
(define bg-color utah-granite)

(define *export* (make-parameter #false))

(define bbox-frame-color (make-parameter utah-crimson))
(define bbox-radius (make-parameter 1))
(define bbox-x-margin (make-parameter small-x-sep))
(define bbox-y-margin (make-parameter tiny-y-sep))
(define bbox-frame-width (make-parameter 2))

(define (color-off c)
  (color%-update-alpha c 0.2))

(define title-font "Montserrat" #;"Bree Serif")
(define body-font "Source Sans Pro" #;"Open Sans")
(define code-font "Inconsolata")

(define title-size 42)
(define subtitle-size 32)
(define head-size 38)
(define body-size 40)
(define code-size 32)
(define tcode-size (- code-size 4))

(define ((make-string->text #:font font #:size size #:color color) . str*)
  (colorize (text (apply string-append str*) font size) color))

(define (bold-style font)
  (cons 'bold font))

(define (italic-style font)
  (cons 'italic font))

(define body-font-lo (make-object font% body-size body-font 'default 'normal 'light))
(define body-font-it (make-object font% body-size body-font 'default 'italic 'light))
(define body-font-itbf (make-object font% body-size body-font 'default 'italic 'semibold))
(define body-font-md (make-object font% body-size body-font 'default 'normal 'medium))
(define body-font-hi (make-object font% body-size body-font 'default 'normal 'semibold))
(define utah-web-headline-font (make-object font% title-size title-font 'default 'normal 'semibold))
(define page-font (make-font #:face code-font #:size tcode-size))

(define titlerm (make-string->text #:font utah-web-headline-font #:size title-size #:color black))
(define titlerm2 (make-string->text #:font utah-web-headline-font #:size (- title-size 8) #:color black))
(define subtitlerm (make-string->text #:font title-font #;body-font-md #:size subtitle-size #:color black))
(define subtitlermem (make-string->text #:font (bold-style title-font) #:size subtitle-size #:color emph-color))
(define subtitlermemlo (make-string->text #:font title-font #:size subtitle-size #:color emph-color))
(define subtitlermlo
  (let ((ff (make-string->text #:font title-font #:size subtitle-size #:color black)))
    (lambda str*
      (cellophane (apply ff str*) 0.7))))
(define headrm (make-string->text #:font title-font #:size head-size #:color dark-blue))
(define coderm (make-string->text #:font code-font #:size code-size #:color black))
(define codebf (make-string->text #:font (bold-style code-font) #:size code-size #:color black))
(define codeemrm (make-string->text #:font (bold-style code-font) #:size code-size #:color green2-3k1))
(define greenrm codeemrm)
(define codeemrm2 (make-string->text #:font (bold-style code-font) #:size code-size #:color emph-color))
(define codeembf (make-string->text #:font (bold-style code-font) #:size code-size #:color apple-red))
(define redrm codeembf)
(define tcoderm (make-string->text #:font code-font #:size tcode-size #:color black))
(define tcodebf (make-string->text #:font (bold-style code-font) #:size tcode-size #:color black))
(define tt coderm)

(define bodyrmhi (make-string->text #:font body-font-md #:size body-size #:color black))
(define hugerm (make-string->text #:font body-font-md #:size (+ 20 body-size) #:color black))
(define bodyrmlo (make-string->text #:font body-font-lo #:size body-size #:color black))
(define bodyrm bodyrmlo)
(define rm bodyrmlo)
(define rmlo rm)
(define rmhi bodyrmhi)
(define rmem (make-string->text #:font body-font-lo #:size body-size #:color emph-color))
(define bodyrmlobb (make-string->text #:font body-font-lo #:size body-size #:color deep-pen-color))
(define bodyrmloyy (make-string->text #:font body-font-lo #:size body-size #:color shallow-pen-color))
;; (define bodyrmhi (make-string->text #:font body-font-hi #:size body-size #:color black))
(define bodyrmhibb (make-string->text #:font body-font-hi #:size body-size #:color deep-pen-color))
(define bodyrmhiyy (make-string->text #:font body-font-hi #:size body-size #:color shallow-pen-color))
(define bodyit (make-string->text #:font body-font-it #:size body-size #:color black))
(define bodyitbf (make-string->text #:font body-font-itbf #:size body-size #:color black))
(define bodybf (make-string->text #:font (bold-style body-font) #:size body-size #:color black))
(define bodyemit (make-string->text #:font body-font-it #:size body-size #:color emph-color))
(define bodyemrm (make-string->text #:font body-font-md #:size body-size #:color emph-color))
(define bodyrmem bodyemrm)
(define bodyembf (make-string->text #:font (bold-style body-font) #:size body-size #:color emph-color))
(define bodyemrm2 (make-string->text #:font body-font-md #:size body-size #:color green2-3k1))
(define bodyembf2 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color green2-3k1))
(define bodyembf3 (make-string->text #:font (bold-style body-font-md) #:size body-size #:color apple-red))
(define bodyemty (make-string->text #:font body-font-md #:size body-size #:color deep-pen-color))
(define bodyemun (make-string->text #:font body-font-md #:size body-size #:color untyped-color))
(define bodyembl (make-string->text #:font body-font-md #:size body-size #:color blame-color))

(define (at-find-right tag)
  (at-find-pict tag rc-find 'lc #:abs-x pico-x-sep))

(define (arrowhead-pict rad #:color [color black] #:size [size 20])
  (colorize
    (arrowhead size rad)
    color))

(define up-arrow-pict
  (arrowhead-pict (* 1/4 turn) #:color black))

(define right-arrow-pict
  (arrowhead-pict (* 0 turn) #:color black))

(define left-arrow-pict
  (arrowhead-pict (* 1/2 turn) #:color black))

(define down-arrow-pict
  (arrowhead-pict (* 3/4 turn) #:color black))

(define (sky-arrow)
  (define rr (* 1/2 turn))
  (define ss 20)
  (cc-superimpose
    (arrowhead-pict rr #:color (bbox-frame-color) #:size ss)
    (arrowhead-pict rr #:color white #:size (- ss 3))))

(define (author-append . pp*)
  (apply vl-append pico-y-sep pp*))

(define (affiliation-pict)
  (bbox
    (vc-append
      (yblank tiny-y-sep)
      (ht-append smol-x-sep (brown-logo) (meta-logo)))))

(define main-logo-w 200)
(define main-logo-h 100)

(define (brown-logo)
  (main-logo "img/browncs-logo.png"))

(define (meta-logo)
  (main-logo "img/meta-logo.png"))

(define (-bitmap str)
  (define ps
    (if (and (string? str)
             (or (string-prefix? str "img/")
                 (string-prefix? str "src/")))
      (build-path img-dir (substring str 4))
      str))
  (bitmap ps))

(define (main-logo str [ww main-logo-w] [hh main-logo-h])
  (freeze (scale-to-fit (-bitmap str) ww ww)))

(define checker-w 40)

(define (make-checker c)
  (filled-rectangle checker-w checker-w #:draw-border? #f #:color c))

(define (make-checkerboard w h c0 c1)
  (let* ((b0 (make-checker c0))
         (b1 (make-checker c1))
         (b01 (ht-append b0 b1))
         (b10 (ht-append b1 b0))
         (make-row (lambda (pp) (apply ht-append (make-list (+ 1 (quotient (exact-ceiling w) (pict-width pp))) pp))))
         (row (vl-append (make-row b01) (make-row b10))))
    (apply vl-append (make-list (+ 1 (quotient (exact-ceiling h) (pict-height row))) row))))

(define ((slide-assembler/background2 base-assembler make-rect) slide-title slide-vspace slide-pict)
  (define foreground-pict (base-assembler slide-title slide-vspace slide-pict))
  (define background-pict
    (let ((+margin (* 2 margin))
          (-margin (- margin)))
      (inset (make-rect (+ +margin client-w) (+ +margin client-h)) -margin)))
  (cc-superimpose background-pict foreground-pict))

(define (make-solid-bg w h color)
  (let* ((bg (filled-rectangle w h #:color white #:draw-border? #f))
         (fg (filled-rectangle w h #:color color #:draw-border? #f)))
    (cc-superimpose bg fg)))

(define (make-bg w h) (make-solid-bg w h bg-color))

(define bg-orig (current-slide-assembler))
(define bg-bg (slide-assembler/background2 bg-orig make-bg))

(define (bbox pp
              #:color [color white]
              #:x-margin [x-margin #f]
              #:y-margin [y-margin #f]
              #:frame-color [frame-color #f]
              #:frame-width [frame-width #f]
              #:backup? [backup? #f])
  (define xm (or x-margin (bbox-x-margin)))
  (define ym (or y-margin (bbox-y-margin)))
  (define rr (bbox-radius))
  (add-rounded-border
    (if backup?
      (add-rounded-border
        pp
        #:x-margin xm #:y-margin ym #:radius rr
        #:background-color color #:frame-width 0)
      pp)
    #:x-margin (if backup? 0 xm)
    #:y-margin (if backup? 0 ym)
    #:radius rr
    #:background-color (if backup? white color)
    #:frame-width (or frame-width (bbox-frame-width))
    #:frame-color (or frame-color (bbox-frame-color))))

(define profiler-frame-width 6)

(define (bndbox pp)
  (wbox pp #:frame-color (my->brush-color 1) #:frame-width profiler-frame-width))

(define (totalbox pp)
  (wbox pp #:frame-color (my->brush-color 2) #:frame-width profiler-frame-width))

(define (selfbox pp)
  (wbox pp #:frame-color (my->brush-color 3) #:frame-width profiler-frame-width))

(define (sbox pp)
  (bbox pp
        #:x-margin pico-y-sep
        #:y-margin pico-y-sep))

(define (sboxrm . arg*)
  (sbox (apply bodyrm arg*)))

(define (wbox pp #:frame-color [frame-color #f] #:frame-width [frame-width #f])
  (bbox pp
        #:x-margin pico-x-sep
        #:y-margin pico-y-sep
        #:frame-color frame-color
        #:frame-width frame-width))

(define (wboxrm . arg*)
  (wbox (apply bodyrm arg*)))

(define (bboxrm . arg*)
  (bbox (apply bodyrm arg*)))

(struct code-arrow (src-tag src-find tgt-tag tgt-find start-angle end-angle start-pull end-pull style) #:transparent)

(define (add-code-arrow pp arrow
                        #:both [both-arrow #f]
                        #:arrow-size [pre-arrow-size #f]
                        #:line-width [pre-line-width #f]
                        #:color [color #f]
                        #:label [label (blank)]
                        #:x-adjust-label [x-label 0]
                        #:y-adjust-label [y-label 0]
                        #:hide? [hide? #false])
  (define line-width (or pre-line-width default-line-width))
  (define arrow-size (or pre-arrow-size default-arrow-size))
  ((if both-arrow pin-arrows-line pin-arrow-line)
    arrow-size pp
    (let ((src-tag (code-arrow-src-tag arrow)))
      (if (symbol? src-tag) (find-tag pp src-tag) src-tag))
    (code-arrow-src-find arrow)
    (let ((tgt-tag (code-arrow-tgt-tag arrow)))
      (if (symbol? tgt-tag) (find-tag pp tgt-tag) tgt-tag))
    (code-arrow-tgt-find arrow)
    #:line-width line-width
    #:label label
    #:x-adjust-label x-label
    #:y-adjust-label y-label
    #:hide-arrowhead? hide?
    #:style (code-arrow-style arrow)
    #:start-angle (code-arrow-start-angle arrow)
    #:end-angle (code-arrow-end-angle arrow)
    #:start-pull (code-arrow-start-pull arrow)
    #:end-pull (code-arrow-end-pull arrow)
    #:color (or color default-line-color)))

(define (add-code-line pp arrow
                       #:line-width [pre-line-width #f]
                       #:color [color default-line-color]
                       #:label [label (blank)]
                       #:x-adjust-label [x-label 0]
                       #:y-adjust-label [y-label 0]
                       #:hide? [hide? #false])
  (add-code-arrow pp arrow #:arrow-size 0
                  #:line-width pre-line-width #:color color #:label label
                  #:x-adjust-label x-label #:y-adjust-label y-label #:hide? hide?))

(define (add-code-arrows pp #:arrow-size [arrow-size #f] #:color [color #f] . arrow*)
  (add-code-arrows* pp arrow* #:arrow-size arrow-size #:color color))

(define (add-code-arrows* pp* arrow* #:color [color #f] #:arrow-size [arrow-size #f])
  (for/fold ((pp pp*))
            ((arrow (in-list arrow*)))
    (add-code-arrow pp arrow #:color color #:arrow-size arrow-size)))

(define add-code-arrow* add-code-arrows*)

(define (add-code-lines pp #:color [color #f] . arrow*)
  (add-code-line* pp arrow* #:color color))

(define (add-code-line* pp arrow* #:color [color #f])
  (for/fold ((pp pp))
            ((arrow (in-list arrow*)))
    (add-code-line pp arrow #:color color)))

(define (ben-rule w h #:color [color #f])
  (filled-rectangle w h #:color (or color browncs-frame-color) #:draw-border? #f))

(define (bvrule h #:thickness [thickness #f] #:color [color #f])
  (ben-rule (or thickness 1) h #:color color))

(define (bhrule w #:thickness [thickness #f] #:color [color #f])
  (ben-rule w (or thickness 1) #:color color))

(define (scale-to-pict pp bg)
  (scale-to-fit pp (pict-width bg) (pict-height bg)))

(define (add-lang str)
  (string-append "lang/" str))

(define (add-src str)
  (string-append "img/" str))

(define add-img add-src)

(define word-sep 0)

(define (word-append . pp*)
  (apply hb-append word-sep pp*))

(define line-sep2 (+ 2))

(define (left-line-append2 . pp*)
  (left-line-append2* pp*))

(define (left-line-append2* pp*)
  (apply vl-append line-sep2 pp*))

(define (mid-line-append2 . pp*)
  (mid-line-append2* pp*))

(define (mid-line-append2* pp*)
  (apply vc-append line-sep2 pp*))

(define (right-line-append2 . pp*)
  (right-line-append2* pp*))

(define (right-line-append2* pp*)
  (apply vr-append line-sep2 pp*))

(define ll-append left-line-append2)
(define lc-append mid-line-append2)
(define lr-append right-line-append2)

(define line-sep tiny-y-sep)

(define (left-line-append #:sep [sep #f] . pp*)
  (left-line-append* #:sep sep pp*))

(define l-line-append left-line-append)

(define (left-line-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep line-sep) pp*))

(define (mid-line-append #:sep [sep #f] . pp*)
  (apply vc-append (or sep line-sep) pp*))

(define m-line-append mid-line-append)

(define (right-line-append . pp*)
  (apply vr-append line-sep pp*))

(define r-line-append right-line-append)

(define code-line-sep (h%->pixels 12/1000))

(define (code-line-append . pp*)
  (code-line-append* pp*))

(define (code-line-append* pp*)
  (apply vl-append code-line-sep pp*))

(define (codeblock-append #:sep [sep #f] . pp*)
  (codeblock-append* pp*))

(define (codeblock-append* #:sep [sep #f] pp*)
  (apply vl-append (or sep tiny-y-sep) pp*))

(define (hcodeblock-append #:sep [sep #f] . pp*)
  (hcodeblock-append* #:sep sep pp*))

(define (hcodeblock-append* #:sep [sep #f] pp*)
  (apply ht-append (or sep tiny-x-sep) pp*))

(define (scale-lang-lo pp)
  (scale-to-fit pp 120 80))

(define (lang-lo str)
  (scale-lang-lo (-bitmap str)))

(define (symbol->lang-pict sym #:ext [ext #f])
  (lang-lo (add-img (add-lang (format "~a.~a" sym (or ext 'png))))))

(define (split/n lang-img* n)
  (let loop ((pp* lang-img*))
    (if (< (length pp*) n)
      (list pp*)
      (let-values (((a b) (split-at pp* n)))
        (cons a (loop b))))))

(define (X-codeblock pp* #:dark? [dark? #f] #:title [title #f] #:label [label #f] #:frame-color [frame-color #f] #:background-color [background-color #f])
  (define title-pict (if (pict? title) title (if (string? title) (bodyrmlo title) #f)))
  (define label-margin (if title-pict (* 10/100 (pict-height title-pict)) 0))
  (define (add-label-margin pp [extra 0]) (vl-append (+ extra label-margin) (blank) pp))
  (define radius 1)
  (define fw 5)
  (let* ((block-pict
           (bbox
             (code-line-append* pp*)
             #:backup? #t
             #:frame-color #f #;(if dark? #f background-color)
             #:color (if dark?
                       background-color
                       (color%-update-alpha background-color 0.4)))))
    (if label
      (let ((block-pict (add-label-margin block-pict 2)))
        (ppict-do (if title-pict (lt-superimpose block-pict (ht-append 4 (blank) title-pict)) block-pict)
          #:go (coord 1/2 0 'ct) label))
      (if title-pict (vc-append 0 (ht-append 4 (blank) title-pict) (add-label-margin block-pict)) block-pict))))

(define (conslang x y)
  (if x (list* (tt x) (blank) y) y))

(define (untyped-code str)
  (untyped-codeblock #:title #f #:lang #f str))

(define (untyped-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang untyped"] . str*)
  (untyped-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (untyped-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color untyped-pen-color #:background-color untyped-brush-color))

(define (shallow-code str)
  (shallow-codeblock #:title #f #:lang #f str))

(define (shallow-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang shallow"] . str*)
  (shallow-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (shallow-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color shallow-pen-color #:background-color shallow-brush-color))

(define (deep-code str)
  (deep-codeblock #:title #f #:lang #f str))

(define (deep-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang deep"] . str*)
  (deep-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (deep-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color deep-pen-color #:background-color deep-brush-color))

(define typed-codeblock* deep-codeblock*)

(define (concrete-code str)
  (concrete-codeblock #:title #f #:lang #f str))

(define (concrete-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang concrete"] . str*)
  (concrete-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (concrete-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color concrete-pen-color #:background-color concrete-brush-color))

(define (primitive-code str)
  (primitive-codeblock #:title #f #:lang #f str))

(define (primitive-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang primitive"] . str*)
  (primitive-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (primitive-codeblock* pp* #:dark? [dark? #f] #:title [title #f])
  (X-codeblock pp* #:dark? dark? #:title title #:frame-color primitive-pen-color #:background-color primitive-brush-color))

(define (ucode str)
  (untyped-codeblock* (list (coderm str))))

(define (tcode str)
  (typed-codeblock* (list (coderm str))))

(define (ccode str)
  (concrete-codeblock* (list (coderm str))))

(define (untyped-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color untyped-brush-color pp))

(define (typed-box pp)
  (bbox #:x-margin 0 #:y-margin 0 #:color deep-brush-color pp))

(define (typed-codeblock #:dark? [dark? #f] #:title [title #f] #:lang [lang #f #;"#lang typed"] . str*)
  (deep-codeblock* #:dark? dark? #:title title (conslang lang (map tt str*))))

(define (xblank n)
  (blank n 0))

(define (yblank n)
  (blank 0 n))

(define (pblank pp)
  (blank (pict-width pp) (pict-height pp)))

(define (bblur pp #:alpha [alpha #f] #:bg [bg? #f])
  (define fg (cellophane pp (or alpha 4/10)))
  (if bg?
    (cc-superimpose (bgrect fg) fg)
    fg))

(define (bblur2 pp)
  (bblur pp #:alpha 0.7))

(define (maybe-bblur yes? pp)
  (if yes?  (bblur pp) pp))

(define (bgrect pp)
  (brect pp bg-color))

(define (brect pp cc)
  (filled-rectangle (pict-width pp) (pict-height pp) #:draw-border? #f #:color cc))

(define xsep xblank)
(define ysep yblank)

(define (bghost pp)
  (blank (pict-width pp) (pict-height pp)))

(define big-swatch-blank (blank small-y-sep small-y-sep))

(define (untyped-icon-tiny)
  (parameterize ((bbox-x-margin 2) (bbox-y-margin 2))
    (untyped-codeblock* (list (blank 40 40)))))

(define (deep-icon-tiny)
  (parameterize ((bbox-x-margin 2) (bbox-y-margin 2))
    (deep-codeblock* (list (blank 40 40)))))

(define (shallow-icon-tiny)
  (parameterize ((bbox-x-margin 2) (bbox-y-margin 2))
    (shallow-codeblock* (list (blank 40 40)))))

(define (untyped-icon #:lbl [lbl "U"])
  (center-label
    (untyped-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (typed-icon #:lbl [lbl "T"])
  (center-label
    (deep-codeblock* #:title #f (list big-swatch-blank))
    lbl))

(define (center-label pp lbl)
  (ppict-do
    pp
    #:go (coord 1/2 46/100 'cc)
    (if lbl (scale (headrm lbl) 0.9) (blank))))

(define (racket-pict hh)
  (freeze (scale-to-square (bitmap (build-path src-dir "racket.png")) hh)))

(define (label-below base . pp*)
  (vc-append 0 base (apply vc-append 2 pp*)))

(define (label-above base . pp*)
  (vc-append 0 (apply vc-append 2 pp*) base))

(define (python-pict)
  (symbol->lang-pict 'python))

(define (mkchess n)
  (format "img/chess~a.png" n))

(define ((scale-square n) pp)
  (scale-to-square pp n))

;; ---

(define (pay-for-types n)
  (define t-avg
    (typed-codeblock*
      (list
        @tcoderm{avg : Gradebook -> Num}
        @tcoderm{def avg(g):}
        @tcoderm{  return mean(get_column(g, "score"))}
        (blank))))
  (define u-avg
    (untyped-codeblock*
      (list
        @tcoderm{ }
        @tcoderm{def avg(g):}
        @tcoderm{  return mean(get_column(g, "score"))}
        (blank))))
  (define other-code
    (vl-append
      tiny-y-sep
      (untyped-codeblock*
        (list
          @tcoderm{def mean(nums):}
          @tcoderm{  ....}
          (blank)))
      (untyped-codeblock*
        (list
          @tcoderm{def get_column(table, col_name):}
          @tcoderm{  ....}
          (blank)))))
  (define (mk-client ff)
    (let* ((hh (box #f))
           (heq (lambda (pp)
                  (if (unbox hh)
                    (lc-superimpose (xblank (unbox hh)) pp)
                    (begin (set-box! hh (pict-width pp)) pp)))))
      (ff
        (list
          (hc-append tiny-x-sep (heq @tcoderm{avg(quiz_1_grades)}) (check-mini))
          (blank)
          ((if (< n 3) values pblank) (hc-append tiny-x-sep (heq @tcoderm{avg(recipe_book)}) (stop-mini)))
          (blank)
          ((if (< n 3) values pblank) (hc-append tiny-x-sep (heq @tcoderm{avg(42)}) (stop-mini)))))))
  (define u-client (mk-client untyped-codeblock*))
  (define t-client (mk-client typed-codeblock*))
  (ht-append
    tiny-x-sep
    (vl-append
      tiny-y-sep
      (if (< n 1) u-avg t-avg)
      (if (< n 2) other-code (blank)))
    (ppict-do
      (if (< n 3) u-client t-client)
      #:go (coord 1/2 1 'ct #:abs-y tiny-y-sep)
      (if (= n 1) (bbox @rmlo{Add types, code still runs}) (blank)))))

(define (ctc-bnd-pict)
  @rmlo{Contract @|at-sign| boundary})

(define (typed-assert-pict)
  @rmlo{Asserts in typed code})

(define (deep-mag) (mag-icon 80))
(define (shallow-mag) (shuffle-grid (mag-icon 20)))

(define (how-to sym)
  (define-values [r1 r2 r3]
    (case sym
      ((deep) (values (check-mini2) (blank) (blank)))
      ((shallow) (values (stop-mini) (check-mini2) (check-mini2)))
      (else (values (blank) (blank) (blank)))))
  (ptable
    #:ncols 2
    #:col-sep tiny-x-sep
    #:row-sep tiny-y-sep
    (list r1 (ctc-profile)
          r2 (total-profile)
          r3 (self-profile))))

(define (enter-rp n)
  (define p0 (where-to 2))
  (define p1
    (vc-append
      tiny-y-sep
      (wbox
        (vc-append
          pico-y-sep
          (node-append (list uu dd uu ss))
          (word-append @rmhi{Q} @rmlo{. how to find a boundary?})))
      (ht-append tiny-x-sep (ctc-profile) (total-profile) (self-profile))))
  (define p2
    (vc-append
      (bbox (word-append @rmhi{A} @rmlo{. Rational Programmer experiment}))
      (yblank smol-y-sep)))
  (hc-append
    (- tiny-x-sep)
    (vc-append
      smol-y-sep
      p0
      ((if (< n 1) pblank values) p1))
    ((if (< n 2) pblank values) p2)))

(define (do-migrate pp0 pp1)
  (let* ((pp (hc-append smol-x-sep (add-hubs pp0 'pp0) (add-hubs pp1 'pp1)))
         (arr (code-arrow 'pp0-E rc-find 'pp1-W lc-find 0 0 0 0 'solid))
         (pp (add-code-arrow pp arr)))
    pp))

(define (ctc-vs-profile n)
  (define dd
    (bbox
      (hc-append
        tiny-x-sep
        (deep-mag)
        (lc-append
          (word-append @rmhi{Deep} @rmlo{ types})
          (ctc-bnd-pict)))))
  (define ss
    (bbox
      (hc-append
        tiny-x-sep
        (shallow-mag)
        (lc-append
          (word-append @rmhi{Shallow} @rmlo{ types})
          (typed-assert-pict)))))
  (define dd-prf (how-to 'deep))
  (define ss-prf (how-to 'shallow))
  (ht-append
    med-x-sep
    (vc-append smol-y-sep dd ((if (< n 1) pblank values) dd-prf))
    (vc-append smol-y-sep ss ((if (< n 1) pblank values) ss-prf))))

(define (ctc-profile) (bndbox @rmlo{Contract %}))
(define (total-profile) (totalbox @rmlo{Total %}))
(define (self-profile) (selfbox @rmlo{Self %}))

(define (gt-costs n)
  (define top (add-hubs @rmlo{Costs depend ...} 'top #:hub-length tiny-x-sep))
  (define ll
    (if (< n 1)
      (lc-append
        @rmlo{Guarded semantics}
        (word-append @rmlo{(} @rmhi{deep} @rmlo{ types)})
        (yblank pico-y-sep)
        (ctc-bnd-pict))
      (lc-append
        @rmhi{deep}
        @rmlo{check full gradebook}
        (yblank pico-y-sep)
        @redrm{9x})))
  (define rr
    (if (< n 1)
      (lc-append
        @rmlo{Transient semantics}
        (word-append @rmlo{(} @rmhi{shallow} @rmlo{ types)})
        (yblank pico-y-sep)
        (typed-assert-pict))
      (lc-append
        @rmhi{shallow}
        @rmlo{check book shape, numbers}
        (yblank pico-y-sep)
        @greenrm{~1x})))
  (define ll-pict (deep-mag))
  (define rr-pict (shallow-mag))
  (define pp
    (bbox
      (vc-append
        (h%->pixels 4/100)
        top
        (ht-append bigg-y-sep
                   (hc-append tiny-x-sep ll-pict ll)
                   (hc-append tiny-x-sep rr-pict rr)))))
  (add-code-arrow*
    #:color black
    pp
    (list
      (code-arrow 'top-S lb-find ll ct-find (* 3/4 turn) (* 3/4 turn) 4/10  4/10 'solid)
      (code-arrow 'top-S rb-find rr ct-find (* 3/4 turn) (* 3/4 turn) 3/10  2/10 'solid))))

(define (typed-costs n)
  (define ll
    (lc-append
      @rmhi{deep}
      @rmlo{no boundaries!}
      (yblank pico-y-sep)
      @greenrm{1x}))
  (define rr
    (lc-append
      @rmhi{shallow}
      @rmlo{more types, more checks}
      (yblank pico-y-sep)
      @redrm{2x}))
  (define ll-pict (mag-icon 80))
  (define rr-pict (shuffle-grid (mag-icon 20)))
  (define pp
    (bbox
      (ht-append bigg-y-sep
                 (hc-append tiny-x-sep ll-pict ll)
                 (hc-append tiny-x-sep rr-pict rr))))
  pp)

(define (bstripe pp)
  ;; (define color (or -color utah-darkgrey))
  (define +margin (* 2 margin))
  (define fg (cc-superimpose (xblank (+ client-w +margin)) pp))
  (bbox fg))

(define (make-takeaways n)
  (define t1
    (lc-append
      (add-star (word-append @rmhi{contract} @rmlo{ profiling + } @rmhi{deep} @rmlo{ types}))
      (word-append @rmlo{=  } @rmhi{best} @rmlo{ for type migration})))
  (define t2
    (add-star (word-append @rmlo{shallow types do not help})))
  (define t3
    (lc-append
      (add-star (word-append @rmlo{the } @rmhi{rational programmer} @rmlo{ method}))
      (word-append @rmlo{enables rigorous } @rmhi{experiments})))
  (define txt-width (apply max (map pict-width (list t1 t2 t3))))
  (define txt-height (apply max (map pict-height (list t1 t2 t3))))
  (define xshim (blank txt-width tiny-y-sep))
  (vc-append
    smol-y-sep
    (bstripe (tag-pict ((if (< n 2) pblank values) (tlogo #:p (hl-pict txt-height) t3)) 'rational))
    (bstripe (tlogo #:p (ll-pict txt-height)
                    (vl-append t1 xshim (tag-pict ((if (< n 1) pblank values) t2) 'nohelp))))))

(define (future-scale pp)
  (scale pp 8/10))

(define (tlogo pp #:p img)
  (ppict-do
    pp
    #:go (coord 0 0 'rt #:abs-x (- smol-x-sep))
    img))

(define (ll-pict hh)
  (freeze (scale-to-square (bitmap (build-path src-dir "wrench.png")) hh)))

(define (hl-pict hh)
  (scale-to-square
    (lightbulb #:color utah-sunrise)
    hh))

(define (rational-logo)
  (define txt
    (lc-append
      @rmhi{Rational Programmer}
      @rmlo{method  (icfp'21)}))
  (bbox (vc-append tiny-y-sep (hl-pict 111) txt)))

(define (mag-icon [hh 60])
  (bitmap (magnifying-glass-icon #:height hh)))

(define (check-mini)
  (check-pict 40))

(define (check-mini2)
  (check-pict2 40))

(define (stop-mini)
  (stop-pict 30))

(define (stop-mini2)
  (stop-pict2 30))

(define (check-pict2 h)
  (bitmap (check-icon #:height h #:material metal-icon-material)))

(define (check-pict h)
  (bitmap (check-icon #:color apple-green #:height h #:material rubber-icon-material)))

(define (caution-pict h)
  (bitmap (close-icon #:color utah-sunrise #:height h #:material plastic-icon-material)))

(define (stop-pict h)
  (bitmap (stop-icon #:color utah-crimson #:height h #:material plastic-icon-material)))

(define (stop-pict2 h)
  (bitmap (stop-icon #:color utah-crimson #:height h #:material metal-icon-material)))

(define (shuffle-grid pp)
  (define row* (make-list 3 pp))
  (define sep 4)
  (apply
    vc-append
    sep
    (make-list 3 (apply hc-append sep row*))))

(define (popl-problem n)
  (define paper (sbox (freeze (scale-to-square (bitmap "src/deadhorse.png") (w%->pixels 4/10)))))
  (define lbl
    (ptable
      #:ncols 2
      #:row-sep 0
      #:col-sep pico-y-sep
      (list @rmlo{popl'16:  } @rmlo{10x slowdowns are common,}
            (blank)           (word-append @rmhi{but} @rmlo{  fast points exist!}))))
  (define low
    ((if (< n 1) pblank values) (bbox (hc-append tiny-x-sep (mag-icon) @rmlo{How to find??}))))
  (define rp (rational-logo))
  (hc-append
    smol-x-sep
    (ppict-do
      (vc-append tiny-y-sep (bbox lbl) paper)
      #:go (coord 1/2 1 'ct #:abs-y (+ tiny-y-sep))
      low)
    ((if (< n 2) pblank values) rp)))

(define (casts-and-costs n)
  (define ll
    (bbox
      (ll-append
        (word-append @rmlo{How to avoid } (tag-pict @rmhi{runtime costs} 'costs))
        (yblank pico-y-sep)
        (word-append @rmlo{using } (tag-pict @rmhi{off-the-shelf tools} 'tools) @rmlo{?}))))
  (define rr
    (bbox
      (ptable
        #:ncols 3
        #:row-sep pico-y-sep
        #:col-sep 0
        (list
          @rmhi{costs} @rmlo{ ~ } @rmlo{gradual types}
          (yblank pico-y-sep) (blank) (blank)
          @rmhi{tools} @rmlo{ ~ } @rmlo{statistical profilers}))))
  (hc-append smol-x-sep ll ((if (< n 1) pblank values) rr)))

(define (fsm-profile-table)
        (ptable
          #:row-sep med-y-sep
          #:col-sep tiny-y-sep
          (list
            (fsm-append
              (list
                (untyped-icon-tiny)
                (deep-icon-tiny)
                (untyped-icon-tiny)
                (untyped-icon-tiny)))
            @bodyrm{deep = slow at boundaries}
            (fsm-append
              (list
                (untyped-icon-tiny)
                (shallow-icon-tiny)
                (untyped-icon-tiny)
                (untyped-icon-tiny)))
            @bodyrm{shallow = slow in typed code}
          )))

(define (strategies-pict [n 0])
  ;; TODO table
  (define mkrow (lambda (a b) (hc-append 8 a b)))
  (define row*
    (list
        (mkrow @bodyrmhi{opt}
               @bodyrmlo{optimistically add deep types})
        (mkrow @bodyrmhi{con}
               @bodyrmlo{conservatively add shallow types})
        (mkrow @bodyrmhi{cost-aware}
               @bodyrmlo{conservatively add shallow types})
        (mkrow @bodyrmhi{config-aware}
               @bodyrmlo{if <50% typed, add shallow, otherwise add deep})
        (yblank smol-y-sep)
        (mkrow @bodyrmhi{null}
               @bodyrmlo{add types randomly})
        (mkrow @bodyrmhi{toggle}
               @bodyrmlo{[pldi'22] change deep types <--> shallow})))
  (define pp*
    (for/list ((rr (in-list row*))
               (ii (in-naturals)))
      ((if (and n (> ii n)) pblank values) rr)))
  (bbox (apply ll-append pp*)))

(define (fsm-append pp*)
  (apply ht-append pico-x-sep pp*))

(define (pre-skylines m-loose)
  (define lbl (loose-label m-loose))
  (X-skylines lbl m-loose))

(define (skylines m-loose)
  (define lbl (loose-label* m-loose))
  (X-skylines lbl m-loose))

(define (X-skylines lbl m-loose)
  (define pp
    (sbox (freeze (scale-to-fit (-bitmap (format "img/strategy-overall-~afeasible.png" (or m-loose ""))) 1000 1000))))
  (define ll (sbox (freeze (scale (-bitmap "img/legend.png") 13/10))))
  (vc-append
    pico-y-sep
    lbl pp ll))

(define max-loose-N 5)

(define (loose->int x)
  (cond
    [(exact-integer? x) x]
    [(eq? 'N x) 4]
    [else max-loose-N]))

(define (loose->string x)
  (case x
    ((-2) " X = strategies, Y = % scenarios")
    ((-1) "example data")
    ((0) "strict success")
    ((1) "1 loose")
    ((2) "2 loose")
    ((3) "3 loose")
    ((N) "N loose")
    (else "strict 3x")))

(define (int->loose n)
  (case n
    ((-2) -2)
    ((-1) -1)
    ((0) 0)
    ((1) 1)
    ((2) 2)
    ((3) 3)
    ((4) 'N)
    (else #f)))

(define (loose-label* m-loose)
  (define N-loose (loose->int m-loose))
  (apply
    hc-append
    pico-x-sep
    (for/list ((ii (in-range (+ 1 max-loose-N))))
      (define ff
        (cond
          [(< ii N-loose) bblur]
          [(= ii N-loose) values]
          [else pblank]))
      (ff (wbox (label-scale (rmlo (loose->string (int->loose ii)))))))))

(define (loose-label m-loose)
  (define N-loose (loose->int m-loose))
  (define inner (loose->string m-loose))
  (if inner (wbox (label-scale (rmlo inner))) (blank)))

(define (label-scale pp)
  (scale pp 9/10))

(define (comment-scale pp)
  (scale pp 8/10))

(define (rational-programmer-idea [n 0])
  (vc-append
    smol-y-sep
    (bbox
      (ll-append
        @bodyrm{Simulate idealized programmers}
        (yblank smol-y-sep)
        @bodyrm{Recipe:}
        (yblank pico-y-sep)
        @bodyrm{- find problem scenarios}
        @bodyrm{- apply a fix, repeat}
        @bodyrm{- search for a goal}))
    (bbox
      (apply hc-append 4 (make-list 6 right-arrow-pict )))))

(define (profile-pict sym)
  (freeze (scale-to-fit (bitmap (build-path src-dir (format "~a-example.png" sym))) (w%->pixels 44/100) (h%->pixels 60/100))))

(define (stat-profile-pict)
  (define cc-total (my->brush-color 2))
  (define cc-self (my->brush-color 2))
  (ppict-do
    (profile-pict 'stat)
    #:go (coord 21/100 22/100 'cc)
    (totalswatch)
    #:go (coord 43/100 22/100 'cc)
    (selfswatch)))

(define (sample-profile n)
  (define profile-bitmap (stat-profile-pict))
  (define bnd-bitmap (profile-pict 'bnd))
  (define xshim (xblank (max (pict-width profile-bitmap) (pict-width bnd-bitmap))))
  (define stat-pp
    (case n
      ((0) (blank))
      ((1) profile-bitmap)
      (else (hc-append
              smol-x-sep
              (totalbox @rmlo{Total %})
              (selfbox @rmlo{Self %})))))
  (define bnd-pp
    (case n
      ((0 1 2) (blank))
      ((3) bnd-bitmap)
      (else (bndbox @rmlo{Contract %}))))
  (ht-append
    med-x-sep
    (vc-append
      pico-y-sep
      (cc-superimpose xshim @bboxrm{Statistical Profiler})
      stat-pp)
    (vc-append
      pico-y-sep
      (cc-superimpose xshim @bboxrm{Contract Profiler})
      bnd-pp)))

(define (path-append . pp*)
  (path-append* pp*))

(define (path-append* pp*)
  (apply
    hc-append
    tiny-x-sep
    (add-between pp* right-arrow-pict)))

(define (two-lattice-pict [n 0] #:num-module [num-mod 4])
  (define (bscale pp) (scale pp 1/2))
  (define uu (bscale (untyped-icon-tiny)))
  (define dd (bscale (deep-icon-tiny)))
  (define ss (bscale (shallow-icon-tiny)))
  (define (node-append pp*)
    (apply ht-append 2 pp*))
  (define (mk2 b*)
    (node-append
      (for/list ((b (in-list b*)))
        (if b dd uu))))
  (define (mk3 b*)
    (define num-typed (for/sum ((b (in-list b*)) #:when b) 1))
    (values ;;scale-to-fit
      (make-lattice
        #:x-margin 2 #:y-margin 2
        num-typed
        (lambda (bb*)
          (node-append
            (for/list ((b (in-list b*)))
              (if b
                (begin0
                  (if (car bb*) dd ss)
                  (set! bb* (cdr bb*)))
                uu)))))))
  (define (lhs)
    (bbox (label-above
      (make-lattice num-mod mk2 #:x-margin 4 #:y-margin 4)
      @bodyrm{@~a[(expt 2 num-mod)] typed/untyped configs.})))
  (define (rhs)
    (bbox (label-above
      (make-lattice num-mod mk3 #:x-margin 6 #:y-margin 4)
      @bodyrm{@~a[(expt 3 num-mod)] deep/shallow/untyped configs.})))
  (case n
    ((0) (lhs))
    (else (ppict-do
            (lhs)
            #:go (coord 1/2 35/100 'ct)
            (rhs)))))

(define (bscale pp) (scale pp 1/2))
(define uu (bscale (untyped-icon-tiny)))
(define dd (bscale (deep-icon-tiny)))
(define ss (bscale (shallow-icon-tiny)))
(define (node-append* . pp*)
  (node-append pp*))
(define (node-append pp*)
  (apply ht-append 2 pp*))

(define (where-to n)
  (define base (add-hubs (node-append (list dd uu)) 'base))
  (define o1   (add-hubs (node-append (list ss uu)) 'o1))
  (define o2   (add-hubs (node-append (list dd ss)) 'o2))
  (define o3   (add-hubs (node-append (list dd dd)) 'o3))
  (define pp
    (bbox
      (vc-append
        smol-y-sep
        ((if (< n 1) pblank values) (hc-append tiny-x-sep o1 o2 o3))
        (vc-append
          (ppict-do base #:go (coord 0 1/2 'rc #:abs-y -2) (if (< n 2) @redrm{9x} (blank)))
          (word-append @rmhi{Q} @rmlo{. where to?})))))
  (define the-pull 1/8)
  (define the-angle (* 1/4 turn))
  (define the-style 'solid)
  (if (< n 1)
    pp
    (add-code-arrow*
      pp
      (list
        (code-arrow 'base-N lt-find 'o1-S cb-find the-angle the-angle the-pull the-pull the-style)
        (code-arrow 'base-N ct-find 'o2-S cb-find the-angle the-angle the-pull the-pull the-style)
        (code-arrow 'base-N rt-find 'o3-S cb-find the-angle the-angle the-pull the-pull the-style)))))

(define (type-aware-src)
           (ptable
                  #:ncols 2
                  #:col-sep 2 #:row-sep 0
                  (list @tcoderm{1.} (node-append* dd ss)
                        @tcoderm{2.} (hc-append (node-append (list dd uu)) @rm{ / } (node-append* ss uu)))))

(define (greedy-src)
  (hc-append (node-append* dd uu) @rm{ / } (node-append* ss uu) @rm{ / } (node-append* dd ss)))

(define (once-again pp)
  (ppict-do (pblank pp) #:go (coord 0 0 'lt) @rmlo{...}))

(define (mkstrat name src tgt)
  (mkstrat-plain name (do-migrate src tgt)))

(define (mkstrat-plain name pp)
  (vl-append
    (- pico-y-sep)
    name
    (hc-append (xblank pico-x-sep) pp)))

(define (make-swatch cc)
  (define ww 20)
  (bbox (blank ww ww)
        #:x-margin 0
        #:y-margin 0
        #:color cc #:backup? #f #:frame-color cc))

(define (bndswatch) (make-swatch (my->brush-color 1)))
(define (totalswatch) (make-swatch (my->brush-color 2)))
(define (selfswatch) (make-swatch (my->brush-color 3)))

(define (strategies-pictx n)
  (define greedy-deep
    (let* ((name (hc-append
                   @rmhi{Deep}
                   @rmlo{ ( } (bndswatch) @rmlo{ } (totalswatch) @rmlo{ } (selfswatch) @rmlo{ ) } ))
           (src (greedy-src))
           (tgt (node-append (list dd dd))))
      (mkstrat name src tgt)))
  (define greedy-shallow
    (let* ((name @rmhi{Shallow})
           (src (once-again (greedy-src)))
           (tgt (node-append (list ss ss))))
      (mkstrat name src tgt)))
  (define type-aware-deep
    (let* ((name @rmhi{Type-Aware Deep})
           (src (type-aware-src))
           (tgt (node-append (list dd dd))))
      (mkstrat name src tgt)))
  (define type-aware-shallow
    (let* ((name @rmhi{Type-Aware Shallow})
           (src (once-again (type-aware-src)))
           (tgt (node-append (list ss ss))))
      (mkstrat name src tgt)))
  (define lattice-aware
    (let* ((name (rmhi "Lattice[S; D]"))
           (txt @rmlo{ count #typed, choose Deep or Shallow}))
      (word-append name txt)))
  (define baselines
    (word-append
      @rmhi{null} @rmlo{, } @rmhi{pldi22} @rmlo{ = baselines}))
  (bbox
    (vl-append
      tiny-y-sep
      (ptable
        #:ncols 2
        #:col-sep tiny-x-sep
        #:row-sep tiny-y-sep
        (delay-list n
          greedy-deep greedy-shallow
          type-aware-deep type-aware-shallow))
      ((if (< n 4) pblank values) lattice-aware)
      ((if (< n 5) pblank values) baselines))))

(define (delay-list n . pp*)
  (delay-list* n pp*))

(define (delay-list* n pp*)
  (for/list ((pp (in-list pp*))
             (ii (in-naturals)))
    ((if (< n ii) pblank values) pp)))

(define (three-lattice n #:num-module [num-mod 4] #:lattice-only? [lo? #false])
  (define (my-above top bot) (vc-append pico-y-sep top bot))
  (define -lhs
    (bbox
      (ht-append
        smol-x-sep
        (hc-append
          (my-above
            @rmlo{@~a[num-mod] modules}
            (node-append (list uu uu)))
          (xblank tiny-x-sep)
          right-arrow-pict
          (xblank tiny-x-sep)
          (ht-append
            (my-above
              @rmlo{deep}
              dd)
            @rmlo{  or  }
            (my-above
              @rmlo{shallow}
              ss))))))
  (define lhs
     (if (< n 2)
       (ppict-do -lhs #:go (coord 1 1/2 'lc #:abs-x tiny-x-sep) (label-scale @bboxrm{(pldi'22)}))
       -lhs))
  (define node-sep (if (= num-mod 2) 12 6))
  (define (mk2 b*)
    (node-append
      (for/list ((b (in-list b*)))
        (if b dd uu))))
  (define (mk3 b*)
    (define num-typed (for/sum ((b (in-list b*)) #:when b) 1))
    (values ;;scale-to-fit
      (make-lattice
        #:x-margin node-sep #:y-margin node-sep
        num-typed
        (lambda (bb*)
          (node-append
            (for/list ((b (in-list b*)))
              (if b
                (begin0
                  (if (car bb*) dd ss)
                  (set! bb* (cdr bb*)))
                uu)))))))
  (define rhs
    (tag-pict
      (bbox
        (let ((the-lat (make-lattice num-mod mk3 #:x-margin node-sep #:y-margin node-sep)))
          (if lo? the-lat (label-above the-lat @bodyrm{@~a[(expt 3 num-mod)] points}))))
      'lattice-rhs))
  (if lo?
    rhs
    (vc-append smol-y-sep lhs ((if (< n 1) pblank values) rhs))))

(define (add-star txt)
   (ppict-do txt #:go (coord 0 65/100 'cc #:abs-x (- pico-x-sep)) @rmlo{*}))

;; -----------------------------------------------------------------------------

(define the-title-str "How Profilers Can Help Navigate Type Migration")

(define (title-pict) (bbox (rmhi the-title-str)))

(define (sec:title)
  (unless (*export*)
    ;; TODO why no image??! unreliable
    (pslide
      #:go center-coord
      (freeze (-bitmap (build-path img-dir "chess2.png")))))
  (pslide
    #:go center-coord
    (freeze (-bitmap (build-path img-dir "chess2.png")))
    #:go title-coord-m
    (let* ((top (title-pict))
           (bot (bbox @rmlo{oopsla'23}))
           (xgap (xblank smol-x-sep))
           (mid (bbox
                  (author-append
                    (add-star @rmlo{Ben Greenman})
                    @rmlo{Matthias Felleisen}
                    @rmlo{Christos Dimoulas})))
           )
      (vr-append
        pico-y-sep
        (hc-append xgap top xgap)
        (vc-append pico-y-sep mid bot)))
    )
  (void))

(define (sec:take2)
  (pslide
    #:go title-coord-m
    (title-pict)
    (yblank smol-y-sep)
    #:next
    #:alt ( (casts-and-costs 0) )
    (casts-and-costs 1)
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{Old Problem, New Idea}
    (yblank smol-y-sep)
    #:next 
    #:alt ( (popl-problem 0) )
    #:alt ( (popl-problem 1) )
    (popl-problem 2)
    )
  (pslide
    ;; gradual types, runtime costs
    ;; avg : Gradebook -> Real
    ;; 1. untyped code, some good some error calls
    ;; 2. types on avg, no static error, check at runtime
    ;; 3. slow. how slow depends on semantics = guarded (on nat) semantics vs transient semantics
    ;;    guarded = deep checks at boundaries
    ;;     => table mutable, even more expensive get wrapper check reads and writes
    ;;    transient = shallow checks at boundaries, and throughout typed code
    ;;     => mutable don't matter
    #:go heading-coord-m
    @bboxrm{Gradual Types + Costs}
    (yblank smol-y-sep)
    #:next
    #:alt ( (pay-for-types 0) )
    #:alt ( (pay-for-types 1) )
    (pay-for-types 2)
    #:next
    (yblank tiny-y-sep)
    (bbox (hc-append tiny-x-sep @rmlo{Type soundness} left-arrow-pict @rmlo{Runtime checks}))
    #:next
    (yblank tiny-y-sep)
    #:alt ( (gt-costs 0) )
    (gt-costs 1)
  )
  (pslide
    #:go heading-coord-m
    (pblank @bboxrm{Gradual Types + Costs})
    (yblank smol-y-sep)
    #:alt ( (pay-for-types 2) )
    (pay-for-types 3)
    #:next
    (yblank tiny-y-sep)
    (typed-costs 1)
  )
  (pslide
    #:go center-coord
    #:alt ( (three-lattice 0 #:num-module 2) )
    (three-lattice 1 #:num-module 2)
    #:next
    #:go (at-find-pict 'lattice-rhs rc-find 'lc #:abs-x tiny-x-sep)
    (where-to 1)
    )
  (pslide
    #:go center-coord
    #:alt ( (three-lattice 2 #:num-module 3) )
    (three-lattice 2 #:num-module 4)
    #:next
    #:go (at-find-pict 'lattice-rhs ct-find 'ct #:abs-y (- pico-y-sep))
    (where-to 1)
    (yblank pico-y-sep)
    @bboxrm{Can profilers help?}
    )
  (pslide
    ;; Profilers, Outputs
    ;; 2 (right = stat; left = ctc)
    ;; --> 3 outputs (colorize)
    ;; --> N questions: top suggestion? top untyped? // add types? swap semantics? 
    ;;     // better than random chance?
    #:go heading-coord-m
    (bbox (hc-append tiny-x-sep @rmlo{Profilers} (racket-pict 50)))
    #:next
    (yblank tiny-y-sep)
    (sbox (node-append (list uu dd uu ss)))
    (yblank smol-y-sep)
    #:alt ( (sample-profile 0) )
    #:alt ( (sample-profile 1) )
    #:alt ( (sample-profile 2) )
    #:alt ( (sample-profile 3) )
    (sample-profile 4)
    )
  (pslide
    #:go center-coord
    (ctc-vs-profile 1)
    )
  (pslide
    ;; mountain of Q's -> 2015 problem, unclear how to proceed systematically
    ;; rational programmer, turn into _useful_ experiment
    #:go center-coord
    #:alt ( (enter-rp 1) )
    (enter-rp 2)
    )
  (pslide
    #:go heading-coord-m
    (bbox @rmhi{Rational Programmer})
    (yblank tiny-y-sep)
    #:next
    (bbox @rmlo{Identify strategies, let them compete.})
    #:next
    (yblank tiny-y-sep)
    #:alt ( (strategies-pictx 0) )
    #:alt ( (strategies-pictx 1) )
    #:alt ( (strategies-pictx 2) )
    #:alt ( (strategies-pictx 3) )
    #:alt ( (strategies-pictx 4) )
    (strategies-pictx 5)
    )
  (pslide
    #:go heading-coord-m
    (bbox @rmhi{Rational Programmer})
    (yblank tiny-y-sep)
    (bbox @rmlo{Identify strategies, let them compete.})
    #:next
    (yblank smol-y-sep)
    (ht-append
      smol-x-sep
      (three-lattice 2 #:num-module 3 #:lattice-only? #true)
      (bbox
        (lc-append
          @rmlo{For all starting points,}
          (tag-pict (word-append @rmlo{Goal = } @rmhi{path} @rmlo{ to a fast config}) 'path))))
    #:next
    #:go (at-find-pict 'path cb-find 'ct #:abs-y tiny-y-sep)
    (bbox
      (vc-append
        tiny-y-sep
        (word-append @rmhi{strict} @rmlo{ = faster each step})
        (word-append @rmlo{k } @rmhi{loose} @rmlo{ = k slower steps})))
    (yblank tiny-y-sep)
    (bbox
      (vl-append
        tiny-y-sep
        (path-append
          @coderm{99x} @coderm{99x} @coderm{3x} @greenrm{1x})
        (path-append
          @coderm{ 3x} @redrm{99x} @greenrm{1x})))
    )
  (void))

(define (sec:results)
  (pslide
    #:go heading-coord-m
    (bbox @rmlo{Dataset})
    (yblank med-y-sep)
    (hb-append
      pico-x-sep
      (bbox
        (ptable
          #:ncols 2
          #:col-sep tiny-x-sep
          #:row-sep pico-y-sep
          #:col-align (list rc-superimpose lc-superimpose)
          (list @rmlo{16} @rmlo{GTP Benchmarks}
                @rmlo{116 K} @rmlo{starting points}
                @rmhi{1.2 M} @rmlo{measurements}
                @rmhi{5 GB} @rmlo{output}
                @rmlo{10} @rmlo{months on CloudLab})))
      (freeze (scale-to-square (-bitmap "img/cloudlab.png") 111)))
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{How often do the strategies succeed?}
    #:next
    (yblank tiny-y-sep)
    (parameterize ((bbox-x-margin (bbox-y-margin)))
      (bbox (tag-pict (freeze (scale-to-fit (-bitmap "img/nyc.png") (w%->pixels 6/10) (h%->pixels 6/10))) 'empire)))
    #:next
    #:go (at-find-pict 'empire cc-find 'lc #:abs-x (+ -8 smol-x-sep) #:abs-y (- big-y-sep))
    (hc-append 4 (sky-arrow) (wbox (label-scale (rmlo "loose"))))
    #:go (at-find-pict 'empire cc-find 'lc #:abs-x (+ -8 smol-x-sep) #:abs-y (+ smol-y-sep))
    (hc-append 4 (sky-arrow) (wbox (label-scale (rmlo "strict"))))
  )
  (pslide
    #:go heading-coord-m
    @bboxrm{How often do the strategies succeed?}
    (yblank tiny-y-sep)
    #:alt ( (pre-skylines -2) )
    #:alt ( (pre-skylines -1) )
    #:alt ( (skylines 0)
            #:next
            #:go center-coord
            (comment-scale @wboxrm{Contract > Statistical [total or self]})
            (yblank pico-y-sep)
            #:next
            (comment-scale @wboxrm{Total ~= Self})
            (yblank pico-y-sep)
            #:next
            (comment-scale @wboxrm{Deep >> Shallow})
            (yblank pico-y-sep)
            #:next
            (comment-scale @wboxrm{type-aware, lattice-aware make little difference})
           )
    #:alt ( (skylines 1) )
    #:alt ( (skylines 2) )
    #:alt ( (skylines 3) )
    #:alt ( (skylines 'N)
            #:next
            #:go center-coord
            (comment-scale @wboxrm{Looseness helps a bit})
           )
    (skylines #f)
    #:next
    #:go center-coord
    (comment-scale @wboxrm{3x success helps Shallow})
  )
  #;(pslide
    #:go center-coord
    (bbox
      (lc-append
        @bodyrmhi{pretty bad!}
        @bodyrm{(per-benchmark results vary, see paper)}))
    )
  (void))

(define (sec:takeaways)
  (pslide
    #:go heading-coord-m
    (bbox @rmlo{Takeaways})
    #:next
    (yblank medd-y-sep)
    #:alt ( (make-takeaways 0) )
    #:alt ( (make-takeaways 1)
            #:next
            #:go (at-find-pict 'nohelp rc-find 'lc #:abs-x tiny-x-sep #:abs-y smol-y-sep)
            (wbox (future-scale @rmlo{Q. hybrid strategies, shallow profilers?}))
          )
    (make-takeaways 2)
    #:next
    #:go (at-find-pict 'rational rc-find 'lc #:abs-x tiny-x-sep)
    (wbox
      (future-scale
        (ptable
          #:ncols 2
          #:col-sep tiny-x-sep
          #:row-sep pico-y-sep
          #:col-align cc-superimpose
          (list @rmlo{errors} @rmlo{testing?} @rmlo{perf} @rmlo{debugging?}))))
    )
  (void))

(define (sec:extra)
  (pslide
    #:go center-coord
    (bbox @coderm{https://github.com/bennn/rational-deep-shallow})
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{Translation: talk -> paper}
    (yblank tiny-y-sep)
    (skylines #f)
    #:go center-coord
    (let ((arr "=>")
          (mktxt (lambda (str) (text str 'roman 30))))
      (bbox (ptable
        #:ncols 3
        #:col-sep tiny-x-sep
        #:row-sep pico-y-sep
        (map mktxt
          (list
                "Deep"          "->" "optimistic"
                "type-aware D." "->" "cost-aware optimistic"
                "Shallow"       "->" "conservative"
                "type-aware S." "->" "cost-aware conservative"
                "lattice[S,D]"  "->" "config-aware"
                "rand"          "->" "null"
                "pldi22"        "->" "toggle")))))
  )
  (pslide
    #:go center-coord
    @bboxrm{Skylines per Benchmark}
    (yblank pico-x-sep)
    (bbox
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (-bitmap x) (w%->pixels 43/100) (h%->pixels 75/100))))))
        (ht-append
          tiny-x-sep
          (bbmap "img/skybench-0.png")
          (bbmap "img/skybench-1.png"))))
    )
  (pslide
    #:go center-coord
    @bboxrm{Hopeful Scenarios}
    (yblank pico-x-sep)
    (bbox
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (-bitmap x) (w%->pixels 80/100) (h%->pixels 75/100))))))
        (bbmap "img/hopeful.png")))
    )
  (pslide
    #:go center-coord
    @bboxrm{Opt-Boundary vs. the others}
    (yblank pico-x-sep)
    (bbox
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (-bitmap x) (w%->pixels 80/100) (h%->pixels 75/100))))))
        (bbmap "img/h2h.png")))
    )
  (pslide
    #:go center-coord
    @bboxrm{Where are the Fast Configs?}
    (yblank pico-x-sep)
    (bbox
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (-bitmap x) (w%->pixels 80/100) (h%->pixels 75/100))))))
        (bbmap "img/pyramid.png")))
    )
  (void))

;; -----------------------------------------------------------------------------

(define (do-show)
  ;; (set-page-numbers-visible! #true)
  (set-spotlight-style! #:size 60 #:color (color%-update-alpha highlight-brush-color 0.6))
  ;; [current-page-number-font page-font]
  ;; [current-page-number-color white]
  ;; --
  (parameterize ((*export* (and (member "-x" (vector->list (current-command-line-arguments))) #true))
                 (current-slide-assembler bg-bg))
    (sec:title)
    (sec:take2)
    (sec:results)
    (sec:takeaways)
    (sec:extra)
    (pslide
      #:go heading-coord-m
      (bbox @rmlo{Takeaways})
      (yblank medd-y-sep)
      (make-takeaways 2))
    (when (*export*) (pslide))
    (void))
  (void))

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict)
         ;; (define client-w 984) (define client-h 728) ;; 4:3
         (define client-w 1320) (define client-h 726) ;; 16:9 sort of, too thin
         (define raco-pict
  (ppict-do
    (make-bg client-w client-h)

    #:go center-coord
    (freeze (-bitmap (build-path img-dir "chess2.png")))
    #:go title-coord-m
    (let* ((top (title-pict))
           (bot (bbox @rmlo{oopsla'23}))
           (low (bbox (freeze (scale-to-square (bitmap (build-path src-dir "cra.png")) 140))))
           (xgap (xblank smol-x-sep))
           (mid (bbox
                  (author-append
                    (add-star @rmlo{Ben Greenman})
                    @rmlo{Matthias Felleisen}
                    @rmlo{Christos Dimoulas})))
           )
      (vr-append
        pico-y-sep
        (hc-append xgap top xgap)
        (vc-append pico-y-sep mid bot low)))



  )))
