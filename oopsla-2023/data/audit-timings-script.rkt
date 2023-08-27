#lang racket

(define uncon* (list
;; sieve
(list 0  9)
;; morsecode
(list 0  81)
;; forth
(list 1  81)
;; fsm
(list 0  81)
;; fsmoo
(list 0  81)
;; mbta
(list 0  81)
;; zombie
(list 0  81)
;; dungeon
(list 0  243)
;; jpeg
(list 0  243)
;; lnm
(list 0  729)
;; suffixtree
(list 1  729)
;; kcfa
(list 0  2187)
;; snake
(list 22  6561)
;; take5
(list 0  6561)
;; acquire
(list 0  19683)
;; tetris
(list 388  19683)
;; synth
(list 8  59049)))

(define num-uncon (apply + (map first uncon*)))
(define num-total (apply + (map second uncon*)))

(printf "~a / ~a = ~a%~n" num-uncon num-total (exact->inexact #;exact-round (* 100 (/ num-uncon num-total))))

(define msgap* (list
;; sieve
(list 2  9)
;; morsecode
(list 13  81)
;; forth
(list 27  81)
;; fsm
(list 12  81)
;; fsmoo
(list 8  81)
;; mbta
(list 9  81)
;; zombie
(list 18  81)
;; dungeon
(list 38  243)
;; jpeg
(list 15  243)
;; lnm
(list 638  729)
;; suffixtree
(list 4  729)
;; kcfa
(list 412  2187)
;; snake
(list 6  6561)
;; take5
(list 12  6561)
;; acquire
(list 634  19683)
;; tetris
(list 2166  19683)
;; synth
(list 12  59049)))



(define num-msgap (apply + (map first msgap*)))
(define num-total2 (apply + (map second msgap*)))
(unless (= num-total2 num-total)
  (error 'badtotal))

(printf "~a / ~a = ~a%~n" num-msgap num-total (exact->inexact #;exact-round (* 100 (/ num-msgap num-total))))

