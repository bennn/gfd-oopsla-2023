#lang racket

; This is nice.
; 
; Also it would be good if we could have the diff data combined with the slowdown
; compared to the untyped program and the optimal point in the lattice. In
; essence what I have in mind is a triplet of info for every scenario:
; improvement over starting point x distance from optimal x distance from
; untyped. I think based on that we can formulate three different hypothesis and
; have a graph per benchmark with the scenarios on the x axis, slowdowns on the y
; axis and 1+n points for each scenario (one starting for all strategies and
; final points for each of the n strategies) together with two horizontal lines
; for the untyped and optimal.
; 
; In this context our RQs/hypothesis derive from the following three metrics of
; success where Z1, Z2 and Z3 are thresholds we should pick after looking at the
; data:
; 
; For a given scenario, strategy x is more reduction-successful than strategy y
; iff  it reduces the slowdown by more than Z1 compared to y.
; 
; For a given scenario. Strategy x is more optimal-successful than strategy y iff
; it brings the slowdown more than Z2 closer to the optimal compared to y.
; 
; For a given scenario, Strategy x is more untyped-successful than strategy y iff
; it brings the slowdown more than Z3 closer to the untyped compared to y.
; 
; (the above phrasing needs significant improvement but I hope it communicates
;      the idea here)
; 
; If we are ok with the above, then in terms of graphs we will have the ``raw’’
; data plotted as described above + success summary plots per hypothesis like the
; ones from Lukas’s papers.
