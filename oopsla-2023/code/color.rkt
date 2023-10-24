#lang racket/base

(provide 
  my->pen-color
  my->brush-color)

(require
  pict-abbrevs)

;; ---

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

(define (my->pen-color c)
  (list-ref wong* c))

(define (my->brush-color c)
  #;(color%-update-alpha (my->pen-color c) 0.2)
  (list-ref wong-lite* c))

