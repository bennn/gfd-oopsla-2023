#lang at-exp slideshow

;; 18 min slot
;; 15 min talk 3 min q/a
;; 14:18 ? Thurs October 26 2023
;; ... convert to 4:3 format

;; [X] https://docs.google.com/presentation/d/1vBZkcBu-4AHWRd1AMt6b8OuokaHR91juc6ahOCvOgMU/edit#slide=id.g28099dacf64_0_64
;; [X] simple draft
;; [X] practice talk thursday 11am GOLLY
;; [ ] nice draft
;;  [ ] pict face
;;  [ ] alice in wonderland

(require
  racket/class
  racket/draw
  racket/format
  racket/match
  racket/list
  racket/string
  pict
  ppict/2
  pict-abbrevs
  gtp-pict
  ppict/pict ppict/tag
  pict-abbrevs/slideshow
  plot/no-gui (except-in plot/utils min* max*))

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
(define title-coord-m (coord 1/2 26/100 'ct))
(define all-lang-coord (coord 99/100 1/2 'rc))
(define lesson-coord-h (coord lesson-x hi-text  'lt))
(define lesson-coord-m (coord lesson-x (+ 15/100 hi-text) 'lt))
(define lesson-coord-l (coord lesson-x (+ 30/100 hi-text) 'lt))

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
(define body-size 30)
(define code-size 28)
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
(define codeemrm2 (make-string->text #:font (bold-style code-font) #:size code-size #:color emph-color))
(define codeembf (make-string->text #:font (bold-style code-font) #:size code-size #:color apple-red))
(define tcoderm (make-string->text #:font code-font #:size tcode-size #:color black))
(define tcodebf (make-string->text #:font (bold-style code-font) #:size tcode-size #:color black))
(define tt coderm)

(define bodyrmhi (make-string->text #:font body-font-md #:size body-size #:color black))
(define hugerm (make-string->text #:font body-font-md #:size (+ 20 body-size) #:color black))
(define bodyrmlo (make-string->text #:font body-font-lo #:size body-size #:color black))
(define bodyrm bodyrmlo)
(define rm bodyrmlo)
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
    (arrowhead 20 rad)
    color))

(define up-arrow-pict
  (arrowhead-pict (* 1/4 turn) #:color black))

(define right-arrow-pict
  (arrowhead-pict (* 0 turn) #:color black))

(define left-arrow-pict
  (arrowhead-pict (* 1/2 turn) #:color black))

(define down-arrow-pict
  (arrowhead-pict (* 3/4 turn) #:color black))

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

(define (main-logo str [ww main-logo-w] [hh main-logo-h])
  (freeze (scale-to-fit (bitmap str) ww ww)))

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
    #:frame-width (bbox-frame-width)
    #:frame-color (or frame-color (bbox-frame-color))))

(define (sbox pp)
  (bbox pp
        #:x-margin pico-y-sep
        #:y-margin pico-y-sep))

(define (sboxrm . arg*)
  (sbox (apply bodyrm arg*)))

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
  (scale-lang-lo (bitmap str)))

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

(define (tr-pict)
  (racket-pict))

(define (racket-pict)
  (symbol->lang-pict 'racket))

(define (ts-pict)
  (symbol->lang-pict 'typescript))

(define (flow-pict)
  (symbol->lang-pict 'flow))

(define (typed-clojure-pict)
  (ppict-do
    (symbol->lang-pict 'typed-clojure)
    #:go (coord 65/100 1/2 'cc)
    (clojure-pict)))

(define (clojure-pict)
  (symbol->lang-pict 'clojure))

(define (php-pict)
  (symbol->lang-pict 'php))

;; NOTE room for research / improvement
(define (pyre-pict)
  (symbol->lang-pict 'pyre))

(define (ruby-pict)
  (symbol->lang-pict 'ruby))

(define (strongtalk-pict)
  (symbol->lang-pict 'strongtalk))

(define (typescript-pict)
  (symbol->lang-pict 'typescript))

(define (typed-lua-pict)
  (symbol->lang-pict 'lua)
  #;(ppict-do
    (symbol->lang-pict 'lua)
    #:go center-coord
    @coderm{T. Lua}))

(define (pyret-pict)
  (symbol->lang-pict 'pyret))

(define (dart2-pict)
  (symbol->lang-pict 'dart))

(define (js-pict)
  (symbol->lang-pict 'javascript))

(define (safets-pict)
  (ppict-do
    (js-pict)
    #:go (coord 1/2 0 'ct)
    @coderm{SafeTS}))

(define (strongscript-pict)
  (ppict-do
    (js-pict)
    #:go (coord 1/2 0 'ct)
    @coderm{StrS.}))

(define (thorn-pict)
  (symbol->lang-pict 'thorn))

(define (nom-pict base)
  (define logo
    (freeze
      (ppict-do
        (scale-to-pict (bitmap "img/lang/nom.png") base)
        #:go (coord 0 1 'rb)
        (ben-rule 20 20 #:color white))))
  (define text
    (freeze (scale (bitmap "img/lang/nom-text.png") 9/10)))
  (hc-append text logo))

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
    (sbox (freeze (scale-to-fit (bitmap (format "img/strategy-overall-~afeasible.png" (or m-loose ""))) 880 700))))
  (define ll (sbox (freeze (scale (bitmap "img/legend.png") 13/10))))
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
    ((-2) "x-axis = strategies, y-axis = % scenarios")
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
      (ff (sboxrm (loose->string (int->loose ii)))))))

(define (loose-label m-loose)
  (define N-loose (loose->int m-loose))
  (parameterize ((bbox-x-margin pico-y-sep)
                 (bbox-y-margin pico-y-sep))
    (define inner (loose->string m-loose))
    (if inner (bboxrm inner) (blank))))

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

;; -----------------------------------------------------------------------------

(define the-title-str "How Profilers Can Help Navigate Type Migration")

(define (title-pict)
  (let* ([title-pict
           (bbox
             #:y-margin small-y-sep
             (titlerm the-title-str))]
         [ben-pict
          ;; affiliations? [uu brown] nwu neu
          (author-append
                     @subtitlermem{Ben Greenman}
                     @subtitlermemlo{Matthias Felleisen}
                     @subtitlermemlo{Christos Dimoulas})]
         [author-pict
           (ht-append
             smol-x-sep
             (bbox ben-pict)
             (bbox @subtitlerm{OOPSLA 2023}))]
         [mini-checker
           (make-checkerboard (w%->pixels 8/10) (h%->pixels 2/10) utah-white utah-black)]
         [fg (vc-append medd-y-sep title-pict author-pict mini-checker)])
    (ppict-do
      (pblank fg)
      #:go center-coord fg)))

(define (sec:title)
  (pslide
    #:go title-coord-m
    (title-pict))
  (void))

(define (sec:vision)
  (pslide
    #:go heading-coord-m
    @bboxrm{Sound Gradual Typing}
    (yblank smol-y-sep)
    (bbox
      (ht-append
        0
        (deep-codeblock* (list @bodyrm{typed}))
        @bodyrm{  +  }
        (untyped-codeblock* (list @bodyrm{untyped}))))
    #:next
    (yblank smol-y-sep)
    (ht-append
      med-x-sep
      @bboxrm{Vision: any combo}
      @bboxrm{Reality: some are too slow})
    ;; you are here, in dead zone ... slums
    #:next
    (yblank med-y-sep)
    (bbox
      (ll-append
        ;; @bodyrm{7 years since dead paper, still wondering!}
        @bodyrm{Big improvements in recent years:}
        @bodyrm{- corpse reviver}
        @bodyrm{- pycket}
        @bodyrm{- nom}
        (yblank pico-y-sep)
        @bodyrm{But!  Dead spots remain.}))
    )
  (pslide
    #:go center-coord
    @bboxrm{Q. How to avoid dead spots?}
    (yblank tiny-y-sep)
    @bboxrm{Method: large experiment}
    #:next
    ;; dead = bad typed/untyped configurations ... mabye too much detail for now
    (yblank med-y-sep)
    (bbox
      (ll-append
        @bodyrm{Starting point (pldi'22) deep + shallow types:}
        (ll-append
          @bodyrm{- order-of-magnitude speedups}
          @bodyrm{- conjecture: helpful for navigation})))
    #:next
    (yblank tiny-y-sep)
          ;; @bodyrm{3d lattice, quick solution to the perf problem, huge improvements}
          ;; @bodyrm{navigation _should_ be much more feasible}
          ;; @bodyrm{Today, the reality. Consensus of ben + mf is bogus.}
    (bbox
      (ll-append
        @bodyrm{NOT helpful in our experiment}
        @bodyrm{MUST question science --- including our own!}))
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{Example: dead spots}
    (yblank smol-y-sep)
    (bbox
      (let ((num-mod 4))
        (ht-append
          0
          @bodyrm{FSM program, @~a[num-mod] modules =   }
          (fsm-append (make-list num-mod (untyped-icon-tiny))))))
    (yblank tiny-y-sep)
    #:next
    #:alt (
      (bbox
        (fsm-profile-table)
        ))
    #:next
    #:alt ( (two-lattice-pict 0) )
    #:alt ( (two-lattice-pict 1) )
    (yblank tiny-y-sep)
    #:next
    (bbox
      (lc-append
        @bodyrm{@~a[(expt 3 4)] configs,}
        @bodyrm{60 over 1x,}
        @bodyrm{30 over 3x}))
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{RQ. How to avoid dead spots?}
    (yblank med-y-sep)
    (bbox
      (ll-append
        @bodyrm{lost at sea ....}
        (ptable
          #:row-sep 4 #:col-sep 4
          (map (compose1 (scale-square 220) bitmap mkchess) (build-list 4 values)))))
    (yblank tiny-y-sep)
    @bboxrm{don't forget, adding types is work!}
  )
  #;(pslide
    #:go center-coord
    (bbox
      (ll-append
        @bodyrm{Prior work (maybe skip)}
        @bodyrm{not much, unclear how to proceed}
        @bodyrm{dead horse, k=2 angel steps}
        @bodyrm{Herder: variational --> best (does H need types to run?)}
        @bodyrm{??? any more from paper}))
    )
  (void))

(define (sec:rational)
  (pslide
    #:go center-coord
    @bboxrm{Key Idea: rational programmer}
    #:next
    (yblank smol-y-sep)
    ;; #:alt ( (rational-programmer-idea 0) )
    (rational-programmer-idea 1)
    )
  (pslide
    #:go center-coord
    @bboxrm{Key Tool: off-the-shelf profilers}
    (yblank smol-y-sep)
    (ht-append
      smol-x-sep
      (bbox
        @bodyrm{Boundary profiler})
      (bbox
        @bodyrm{Statistical profiler}))
    (yblank smol-y-sep)
    #:next
    (hc-append
      tiny-y-sep
      (bbox (freeze (scale-to-square (bitmap "img/hiway.png") (w%->pixels 45/100))))
      (bbox (freeze (scale-to-square (bitmap "img/stack.png") (w%->pixels 20/100)))))
    )
  #;(pslide
    #:go center-coord
    @bodyrm{draw modulegraph}
    @bodyrm{draw stack}
  )
  (void))

(define (sec:how)
  (pslide
    #:go center-coord
    (bbox
      (ll-append
        @bodyrm{Toward a rational programmer:}
        @bodyrm{- one scenario = one GT program}
        (yblank smol-y-sep)
        @bodyrm{- goal = <= 1x slowdown}
        (yblank smol-y-sep)
        @bodyrm{- but HOW to "apply a fix"?}
        @bodyrm{  ==> what profiler output?}
        @bodyrm{  ==> how to use the output?}))
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{Boundary profiler output}
    (yblank tiny-y-sep)
    (bbox (freeze (scale-to-square (bitmap "img/bnd-example.png") (w%->pixels 4/10))))
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{Statistical profiler output}
    (yblank tiny-y-sep)
    (bbox (freeze (scale-to-square (bitmap "img/stat-example.png") (w%->pixels 4/10))))
    )
  (pslide
    #:go center-coord
    @bboxrm{Profiler output}
    (yblank smol-y-sep)
    (ht-append
      smol-x-sep
      (vc-append
        tiny-y-sep
        (bbox
          (ll-append
            @bodyrm{Boundary profiler}
            @bodyrm{- slowest contract boundaries})))
      (vc-append
        tiny-y-sep
        (bbox
          (ll-append
            @bodyrm{Statistical profiler}
            @bodyrm{- slowest modules}
            @bodyrm{- dominating modules}))))
    #:next
    (yblank tiny-y-sep)
    (bbox (fsm-profile-table))
    )
  (pslide
    #:go center-coord
    @bboxrm{How to use profiler output?}
    (yblank tiny-y-sep)
    #:next
    (bbox
      (ll-append
        @bodyrm{untyped -> deep}
        @bodyrm{untyped -> shallow}
        @bodyrm{deep -> shallow}
        @bodyrm{shallow -> deep}))
    )
  (pslide
    ;; TODO staging
    #:go heading-coord-m
    @bboxrm{Strategies}
    (yblank smol-y-sep)
    (strategies-pict #f)
    )
  (pslide
    #:go heading-coord-m
    (bbox
      (ll-append
        @bodyrmhi{Instantiate rational programmer}
        @bodyrm{- one scenario = one GT program}
        (yblank smol-y-sep)
        @bodyrm{- 18 ways to make progress, based on:}
        @bodyrm{  ==> 3 kinds of profiler output (based on 2 profilers)}
        @bodyrm{  ==> 4 debugging strategies + 2 baselines}
        (yblank smol-y-sep)
        @bodyrm{- goal = find fast types, <= 1x overhead}
        ))
    )
  (void))

(define (sec:take2)
  (pslide
    ;; title - authors, subtitle how to avoid runtime costs with off-the-shelf tools
    ;; gradual types, statistical profilers (blocks)
    )
  (pslide
    ;; excited b/c dead paper since 2015,
    ;;  problem: 10x slowdown common but fast points exist!
    ;;  !!: nudge toward good points
    ;;  ??: no way to address systematically
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
    ;; uu -> 20x // -> 4x -> 0.9x // 5x -> 0.9x
    ;;  fortunately can mix and match!
    ;; path, untyped to typed uu -> su -> dd
    )
  (pslide
    ;; gradual types, runtime costs
    ;; 3d space, 3^2
    ;; some fast some slow **fast = at least as good as untyped**
  )
  (pslide
    ;; need off-shelf b/c huge search space ... example lattices
    ;; programmer could wind up anywhere!
    ;;  unrealistic to push workload on programmers
    ;; can:
    ;; - run profiler
    ;; - add types
    ;; - swap semantics (guarded <-> transient)
    ;; cannot:
    ;; - remove types (trivial)
    ;; [[ add t questionable! really need auto migration ]]
    ;; context = typed racket
    )
  (pslide
    ;; Profilers, Outputs
    ;; 2 (right = stat; left = ctc)
    ;; --> 3 outputs (colorize)
    ;; --> N questions: top suggestion? top untyped? // add types? swap semantics? 
    ;;     // better than random chance?
    )
  (pslide
    ;; mountain of Q's -> 2015 problem, unclear how to proceed systematically
    ;; rational programmer, turn into _useful_ experiment
    ;; from Q's, derive strategies + baselines
    )
  (pslide
    ;; Instantiation ( How to Compare Strategies )
    ;; 1. consider EVERY configuration as a starting point ... show lattice!
    ;; 2. apply every combination of profile output (show again!) and strategy
    ;;    plus baselines = 15 + 2 = 17 total
    ;; 3. compare: how many successes?
    ;; ** many success:
    ;;     - strict example
    ;;     - 1 loose example
    ;;     - ... 2 loose, ditto
    ;;     - N loose generalize
    )
  ;; next up: experiment + results
  (void))

(define (sec:results)
  (pslide
    #:go center-coord
    (bbox
      (ll-append
        @bodyrmhi{Experiment}
        @bodyrm{- GTP benchmarks, cloudlab}
        @bodyrm{- single-user machines}
        @bodyrm{- 11 runs per config}
        @bodyrm{- 1.2 million total}
        @bodyrm{- artifact on zenodo (275MB compressed)}))
    )
  #;(pslide
    #:go center-coord
    (bbox (bitmap "img/mini-legend.png"))
    )
  (pslide
    #:go heading-coord-m
    @bboxrm{How often do the strategies succeed?}
    #:next
    ;; TODO explain skyscraper = looseness
    (yblank tiny-y-sep)
    (parameterize ((bbox-x-margin (bbox-y-margin)))
      (bbox (freeze (scale-to-fit (bitmap "img/nyc.png") (w%->pixels 6/10) (h%->pixels 6/10)))))
  )
  (pslide
    #:go heading-coord-m
    @bboxrm{How often do the strategies succeed?}
    (yblank tiny-y-sep)
    #:alt ( (pre-skylines -2) )
    #:alt ( (pre-skylines -1) )
    #:alt ( (skylines 0) )
    #:alt ( (skylines 1) )
    #:alt ( (skylines 2) )
    #:alt ( (skylines 3) )
    #:alt ( (skylines 'N) )
    (skylines #f)
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
    (bbox
      (ll-append
        @bodyrm{Takeaways}
        @bodyrm{- opt/boundary is best so far,}
        @bodyrm{  ==> add deep to the slowest boundary}
        (yblank pico-y-sep)
        @bodyrm{- the shallow and deep+shallow strategies are NOT successful}))
    (yblank smol-y-sep)
    #:next
    (bbox
      (ll-append
        @bodyrm{Future Work}
        @bodyrm{- is there a better deep+shallow strategy?}
        (yblank pico-y-sep)
        @bodyrm{- how to profile shallow costs?}))
    (yblank smol-y-sep)
    #:next
    (bbox
      (ll-append
        @bodyrm{The rational programmer method:}
        @bodyrm{  ==> is a powerful way to test design questions}
        @bodyrm{  ==> yet again challenges "obvious" conjectures}))
    )
  (void))

(define (sec:extra)
  (pslide
    #:go center-coord
    @bboxrm{Skylines per Benchmark}
    (yblank pico-x-sep)
    (bbox
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (bitmap x) (w%->pixels 43/100) (h%->pixels 75/100))))))
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
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (bitmap x) (w%->pixels 80/100) (h%->pixels 75/100))))))
        (bbmap "img/hopeful.png")))
    )
  (pslide
    #:go center-coord
    @bboxrm{Opt-Boundary vs. the others}
    (yblank pico-x-sep)
    (bbox
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (bitmap x) (w%->pixels 80/100) (h%->pixels 75/100))))))
        (bbmap "img/h2h.png")))
    )
  (pslide
    #:go center-coord
    @bboxrm{Where are the Fast Configs?}
    (yblank pico-x-sep)
    (bbox
      (let ((bbmap (lambda (x) (freeze (scale-to-fit (bitmap x) (w%->pixels 80/100) (h%->pixels 75/100))))))
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
  (parameterize ((current-slide-assembler bg-bg))
    (pslide)
    (sec:title)
      ;(sec:vision)
      ;(sec:rational)
      ;(sec:how)
;    (sec:take2)
    (sec:results)
    (sec:takeaways)
    (pslide)
    (sec:extra)
    (sec:takeaways)
    (void))
  (void))

(module+ main
  (do-show))

;; =============================================================================

(module+ raco-pict (provide raco-pict)
         (define client-w 984) (define client-h 728) ;; 4:3
         ;; (define client-w 1320) (define client-h 726) ;; 16:9 sort of, too thin
         (define raco-pict
  (ppict-do
    (make-bg client-w client-h)

;    #:go heading-coord-m
;    @bboxrm{Statistical profiler output}
;    (yblank tiny-y-sep)
;    (bbox (freeze (scale-to-square (bitmap "img/stat-example.png") (w%->pixels 4/10))))

    #:go center-coord
    (skylines #f)


  )))
