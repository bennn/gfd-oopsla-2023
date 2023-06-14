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

# Christos's Notes

-----------------------------------------------------------------------------

## 201A -- C

@ The rational programmer

 "In the migration lattice where each configuration is annotated with its
 performance relative to the fully untyped configuration, for each
 configuration where the performance is worse than some threshold, does
 the output of profiler X indicate a (path to a) reachable "next"
 configuration whose performance is either better than the threshold or at
 least better than right now?"

1) The above quote is reductionist. The key is the ``indicate'' part.
Profilers do not suggest actions. They provide feedback that is up to the
programmer to identify. Hence, using the profiler to navigate migration
requires human creativity. The rational programmer is a method for
abstracting systematically such creativity aspects of a work task in order
to establish connections about how a language (semantics) can be used in
a specific context (pragmatics). 

2) The satisficing behavior of the rational programmer is also important
here. We are not looking for optimal paths (I gues this means shortest
paths) or how far strategies of using profilers are from ``optimal''
solutions (whatever that means). This is because a programmer that tries
to use such a tool in practice has no idea about the whole lattice and
can't globally optimize. Instead the programmer has to be satisfied with
reaching a local optimum, meaning a state that is above a desirable
threshold.  

3) The experiment not only matches the ``scheme of the rational
programmer'', the ``scheme of the rational programmer'' provided the
template and the tools for thinking about designing the experiment. 

4) The rational programmer is the first attempt to develop a method for
answering questions of PL pragmatics (such as the question of this paper).
The PL community needs such methods --- otherwise it will keep developing
semantics that do not matter (maybe as gradual type language designers we
should start asking why our designs do not lead to languages that people
use...) Hence, evidence that it applies to a variety of questions is a
valuable contribution on its own right as it is a method that can provide
the ``hard'' facts that can communicate to programmers and language
implementers the value of PL designs. 

5) The reviewer also questions the usefulness of the rational programmer
VS human studies. Human studies do not answer the same questions as the
rational programmer. They would tell us what strategies (if any)
developers tend to use when empolying profilers in a GT setting. Or,
they would tell us if developers can carry out (as have the patience and
discipline) some of the strategies to success. Importantly, due to the
sheer space of vairables that need to be controlled when humans are
involved, a human study would not be a feasible approach to test the
strategies thoroughly. Furthermore, it is a fantasy that a human study
would involve more interesting benchmarks; human studies (exactly to
control variables and lead to some conclusions) are limited to way more
simple programs than the GTP benchmarks. 

@ What we learnt and how the results will be used

For designers: shallow not very good at least with current tools (the
experiment was not doomed; we were expecting to see benefits from shallow)
--- profilers also not as good as they should be to be helpful most times
(at least for shallow) --- profilers seem to hit a fundamental
performance issue with current GT compilation techniques --- as things are
GT language would benefit from feature profilers. All these suggest new
research avenues towards practical GT (and yes langauge designers shoudl
care about tools to, not just semantics because languages without tool
are like kings without clothes)

For developers: prefer deep and the feature profiler --- if you get stuck
then most probably you run into a black hole and it is hopeless to keep
trying with step-wise migration --- if you don't see improvements in
performance, it is questionable whether insisting will turn thigns around.

For language researchers: one of the paper's results is the experimental
design --- researchers can use it to evaluate other languages

@ Strategy design

We considered a space of strategies that revolve around a few specific
knobs (profiler kind/optimistic-conservative). We also supplemented those
with a few compoiste strategies that switch their optimistic-conservative
knob based on some knowledge of the configuration, and two baseline one.
The design of this space derives from 1) developers usually pick a
profiler and stick to it (no tools that combine profilers exist); 2) the
optimistic-conservative dimensions help us isolate the effect of the two
semantics; 3) the composite strategies help as check the effect of
switching between semantics; 4) the baseline ones aim to isolate the
effect of the profiler and create a point of comparison with prior work.
That is ``carefully'' means that the space of strategy was designed in a
systematic ``rational'' manner to examine a spread of possibilities. There
will always be more alternatives and heuristic. But the results seem to
indicate that say combining statistical and boundary won't help due to the
poor resilts of the statistical profiler. In a sense the points in the
space we have selected help us guage how other alterntives would fare, as
it seems the reviewer also can. 

@ Selection of thresholds

The success of TS and the failure of academic approaches tells us that
programmers have little tolerance for slow-downs due to gradual types. The
1x captures that real requirement. The 3x is an upper limit of a more
realistic but not catastrophic slow-down for the current state of affairs.
Notice also we investigate how thigns change with the number of spikes in
slow-down to see if the requirement for constant progress makes sense to
be relaxed.

@ Benchmarks

The GTP benchmarks are not micro-benchmarks. They are a collection of
applications that aim to be representative of different styles of
programming, different language features and different applications. The
bottlenecks they include are not artificially created but arose naturally
during the migration of these projects. The GTP are the only existing
systematic collection of macro-benchmarks for gradual typing. 


