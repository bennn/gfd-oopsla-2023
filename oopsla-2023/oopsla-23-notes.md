# Matthias's Notes

-----------------------------------------------------------------------------

## 201A -- C

```
- Limited Generality
- Inadequate exploration of even the limited scope
- The talk about the "rational programmer" method obfuscates a rather simple methodology
```


@ Running the benchmarks ourselves && question 1 ("How does .. square ..?")

This criticism is fundamentally wrong:

 - Greenman et al. ran the 2^N lattice, not the 3^N lattice. 
 - Greenman PLDI '22 reads extremely encouraging concerning 'shallow'. 
 - But this PLDI paper does _not_ rely on exploring the entire lattice. 

We had to re-run the benchmarks. As the paper shows, exploring the
complete lattices suggests that PLDI '22 is partially flawed. 

@ General result

When someone implements a deep (or shallow) checking regime for a
language with substantial code bases, we will be the first to re-do
this experiment. Racket has run its course here. 


@ "well-known about black holes in the GTP benchmarks" --

This is also a deeply flawed comment. The authors are quite familiar
with the GTP benchmarks and expected a catastrophic outcome, meaning
almost no progress. The paper confirms that the boundary profiler is
successful in _half_ the cases. To the authors, this is a glass half
full -- to be filled with more.

@ GTP benchmarks -- The GTP benchmark suite is a representative
collection of (Typed) Racket programs. It is not a collection of "hard
cases". To claim otherwise requires a presentation of evidence.

As such, this benchmark suite serves as a reasonable starting point.

@ rational programmer && question 4 

The paper is the third in a series that explores the idea of
distilling human behavior into a rational algorithm. As such it is at
least as much an evaluation of this systematic approach as it is an
evaluation of performance.

The review seems to deny that there is value in exploring the
rational-programmer method across different areas of evaluation.
Why?

@ "As someone from that corner [language designers and implementers .. GT]"

The authors of this submission have a combined experience of 43 years
of design experience (including 33 years of implementation experience)
for such languages. This does not include 10 years of experience with
soft-typing systems. 

@ Questions

- #2 ???
- #3 can we see the optimal path?
- #5 like all papers: lots of citations :-)

-----------------------------------------------------------------------------

## 201B -- A

- Wow, someone's familiar with MLKit. But that also means, he might be
  less of an expert than ... Oh, it's James Noble :-)

  (I somehow recall him as not being a great advocate in the past. But
  it might be a faulty memory.)

- I think the questions don't have to be answered but those that are,
  should have easy answers.

- Q2 -- what does he mean here?
- Q4 -- yes, just turn off all checking. See how bad it is.
- Q5 -- no, we really hoped that 'shallow' would help a lot to get out
  of the lower half of the lattices.

-----------------------------------------------------------------------------

## 201C -- B

Q1 "How do these results generalize beyond the walled garden of
   gradually-typed Racket?" 

At the moment, no other research language -- not to speak of an
industrial language comes with the necessary ingredients to repeat
this investigation:

- a 'deep' or 'shallow' backend
- a good amount of code written in this language
- a boundary profiler (and ideally a second one)

Until this set of ingredients is available, we cannot argue on a
scientific basis that these results generalize.

The reverse direction does call for a different form of
generalization. Language implementors of 'deep' or 'shallow' gradually
typed languages should add a boundary profiler. (Instagram's Python
team comes to mind.) This may not be the expected answer but given the
"glass half full" remark above, this is the best answer we have right
now.

Q2 "is "boundary profiler" really an 'approach' in the same sense that
   'statistical profilers' are, or is it a specific artifact?"

It is. As Max New and Ben Greenman's dissertations (both Northeastern)
show, _all_ gradually typed languages can be understood in terms of
boundaries between typed and untyped code. Furthermore, all such
boundaries distribute their performance cost in a way that a
statistical profiler usually doesn't discover. Hence our response to
Q1 is really "implement a boundary profiler" though admittedly, this
may require some more research or at least advanced development on the
side of language implementors.

