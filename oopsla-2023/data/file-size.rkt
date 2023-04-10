#lang racket

; du -h boundary
; 113M	uacquire
; 8.7M	ukcfa
; 17M	ulnm
; 26M	usnake
; 3.0M	usuffixtree
; 235M	usynth
; 27M	utake5
; 78M	utetris
; 348K	uzombie
; 984K	dungeon
; 328K	morsecode
; 40K	sieve
; 328K	fsm
; 328K	forth
; 1.2M	jpeg
; 16M	lnm
; 328K	mbta

'boundary
(+ 
  113
  8.7
  17
  26
  3.0
  235
  27
  78
  ; 348K	uzombie
  1 ; 984K	dungeon
  ; 328K	morsecode
  ; 40K	sieve
  ; 328K	fsm
  ; 328K	forth
  1.2
  16
  1 ; 328K	mbta
  )

; du -h profile
; 631M	uacquire
; 75M	ukcfa
; 46M	ulnm
; 250M	snake
; 57M	usuffixtree
; 2.1G	usynth
; 89M	utake5
; 670M	utetris
; 3.9M	uzombie
; 18M	dungeon
; 1.9M	morsecode
; 220K	sieve
; 2.5M	fsm
; 4.9M	forth
; 11M	jpeg
; 46M	lnm
; 3.4M	mbta

'profile
(+ 
 631
 75
 46
 250
 57
 2100 ; 2.1G	usynth
 89
 670
 3.9
 18
 1.9
 ;220K	sieve
 2.5
 4.9
 11
 46
 3.4)

; ls -lh *out
; -rw-r--r-- 1 ben users  30K Mar 31 16:23 00-morsecode.out
; -rw-r--r-- 1 ben users  31K Sep 27  2022 01-forth.out
; -rw-r--r-- 1 ben users  29K Sep 27  2022 02-fsm.out
; -rw-r--r-- 1 ben users  32K Sep 27  2022 03-fsmoo.out
; -rw-r--r-- 1 ben users  30K Sep 27  2022 04-mbta.out
; -rw-r--r-- 1 ben users 3.5K Sep 27  2022 05-sieve.out
; -rw-r--r-- 1 ben users 7.2M Sep 27  2022 06-acquire.out
; -rw-r--r-- 1 ben users  91K Jan 24 12:28 07-dungeon.out
; -rw-r--r-- 1 ben users  89K Sep 27  2022 08-jpeg.out
; -rw-r--r-- 1 ben users 816K Sep 27  2022 09-kcfa.out
; -rw-r--r-- 1 ben users 255K Sep 27  2022 10-lnm.out
; -rw-r--r-- 1 ben users 2.4M Sep 27  2022 11-snake.out
; -rw-r--r-- 1 ben users 283K Sep 27  2022 12-suffixtree.out
; -rw-r--r-- 1 ben users 2.5M Sep 27  2022 13-take5.out
; -rw-r--r-- 1 ben users 7.3M Jan 24 12:47 14-tetris.out
; -rw-r--r-- 1 ben users  30K Sep 27  2022 15-zombie.out
; -rw-r--r-- 1 ben users  23M Sep 27  2022 19-synth.out

'runtime
(+
;  30K Mar 31 16:23 00-morsecode.out
;  31K Sep 27  2022 01-forth.out
;  29K Sep 27  2022 02-fsm.out
;  32K Sep 27  2022 03-fsmoo.out
;  30K Sep 27  2022 04-mbta.out
; 3.5K Sep 27  2022 05-sieve.out
 7.2
 1
;  91K Jan 24 12:28 07-dungeon.out
;  89K Sep 27  2022 08-jpeg.out
; 816K Sep 27  2022 09-kcfa.out
; 255K Sep 27  2022 10-lnm.out
 2.4
 ; 283K Sep 27  2022 12-suffixtree.out
 2.5
 7.3
 ; 30K Sep 27  2022 15-zombie.out
  23
  )

