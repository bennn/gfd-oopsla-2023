Thanks for the reviews and the extensive comments. We will use them to improve
the paper. For one, we certainly need more background on gradual typing
strategies and their implementation-specific nuances (Shallow JIT, etc).

In the hope of keeping it short, this response focuses on criticisms from #201A:

> ### Cons
> - Limited Generality
> - Inadequate exploration of even the limited scope
> - The talk about the "rational programmer" method obfuscates a rather simple
>   methodology

Some of the comments in the review are quite damning. However, there are
serious misunderstandings at play.

We apologize that our submission was not clear enough about the experiment
and prior work.


#### Limited generality

#201A and #201C raise questions about generality:

> **#201A** [this paper] does not provide a significant improvement over the
> state of the art or provide enough significant insights.

> **#201A**
> the experiments are tailored to Racket and do not obviously generalize to
> other gradually-typed languages, and even there, they do not work that well.

> **#201C**
> Q1: How do these results generalize beyond the walled garden of
> gradually-typed Racket?
>
> Q2: More specifically, is "boundary profiler" really an 'approach' in the
> same sense that "statistical profilers" are, or is it a specific artifact?

While Racket is the only language that 

> CD
> For designers: shallow not very good at least with current tools (the
> experiment was not doomed; we were expecting to see benefits from shallow)
> --- profilers also not as good as they should be to be helpful most times
> (at least for shallow) --- profilers seem to hit a fundamental
> performance issue with current GT compilation techniques --- as things are
> GT language would benefit from feature profilers. All these suggest new
> research avenues towards practical GT (and yes langauge designers shoudl
> care about tools to, not just semantics because languages without tool
> are like kings without clothes)
> 
> For developers: prefer deep and the feature profiler --- if you get stuck
> then most probably you run into a black hole and it is hopeless to keep
> trying with step-wise migration --- if you don't see improvements in
> performance, it is questionable whether insisting will turn thigns around.
> 
> For language researchers: one of the paper's results is the experimental
> design --- researchers can use it to evaluate other languages


> MF
> When someone implements a deep (or shallow) checking regime for a
> language with substantial code bases, we will be the first to re-do
> this experiment. Racket has run its course here. 
> 
> At the moment, no other research language -- not to speak of an
> industrial language comes with the necessary ingredients to repeat
> this investigation:
> 
> - a 'deep' or 'shallow' backend
> - a good amount of code written in this language
> - a boundary profiler (and ideally a second one)
> 
> Until this set of ingredients is available, we cannot argue on a
> scientific basis that these results generalize.
> 
> The reverse direction does call for a different form of
> generalization. Language implementors of 'deep' or 'shallow' gradually
> typed languages should add a boundary profiler. (Instagram's Python
> team comes to mind.) This may not be the expected answer but given the
> "glass half full" remark above, this is the best answer we have right
> now.




#### Inadequate exploration

> the experiment was in some sense doomed from the start

> my main takeaway from this paper are a few more hard numbers on the
> unsurprising fact that there are many low-performing configurations that are
> hard to get out of

> is not really borne out because of the fact that a large chunk of
> configurations - almost half are hopeless to fix in the first place, a fact
> that is well-known, as is the fact that for many of the programs in the GTP
> benchmark suite in Typed Racket, there simply is no reasonable path through the
> migration lattice that avoids unacceptable overheads.
> the bulk of that work was just reproducing the numbers from earlier papers

> you could have also compared against the "optimal" strategy

We did! Figure 5

> , and what other criteria than the more or less randomly chosen 3x overhead
> for success and 1x overhead hopefulness there could be (this is particularly
> noticeable in the `take5` benchmark, which Greenman reports as getting to a
> worst shallow overhead of 2.99x, and a worst combined overhead of 2.97x;
> similarly, `dungeon` has a worst-case overhead of 15000x in with deep types,
> 4.97x with only shallow types, and 3.16x with a combination of the two; again
> close to the boundary; finally, `mbta` has worst-case overheads below 2x all
> the time, though your graphs are still empty because they are all deemed
> hopeless).

Nobody except gradual typing researchers will put up with >1x slowdowns.

If you suggest a threshold, we will use it and attribute to the OOPSLA'23 reviewers.

> TODO strategy design

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



#### Rational programmer obfuscates a simple method



> CD
> 1) The above quote is reductionist. The key is the ``indicate'' part.
> Profilers do not suggest actions. They provide feedback that is up to the
> programmer to identify. Hence, using the profiler to navigate migration
> requires human creativity. The rational programmer is a method for
> abstracting systematically such creativity aspects of a work task in order
> to establish connections about how a language (semantics) can be used in
> a specific context (pragmatics). 
> 
> 2) The satisficing behavior of the rational programmer is also important
> here. We are not looking for optimal paths (I gues this means shortest
> paths) or how far strategies of using profilers are from ``optimal''
> solutions (whatever that means). This is because a programmer that tries
> to use such a tool in practice has no idea about the whole lattice and
> can't globally optimize. Instead the programmer has to be satisfied with
> reaching a local optimum, meaning a state that is above a desirable
> threshold.  
> 
> 3) The experiment not only matches the ``scheme of the rational
> programmer'', the ``scheme of the rational programmer'' provided the
> template and the tools for thinking about designing the experiment. 
> 
> 4) The rational programmer is the first attempt to develop a method for
> answering questions of PL pragmatics (such as the question of this paper).
> The PL community needs such methods --- otherwise it will keep developing
> semantics that do not matter (maybe as gradual type language designers we
> should start asking why our designs do not lead to languages that people
> use...) Hence, evidence that it applies to a variety of questions is a
> valuable contribution on its own right as it is a method that can provide
> the ``hard'' facts that can communicate to programmers and language
> implementers the value of PL designs. 
> 
> 5) The reviewer also questions the usefulness of the rational programmer
> VS human studies. Human studies do not answer the same questions as the
> rational programmer. They would tell us what strategies (if any)
> developers tend to use when empolying profilers in a GT setting. Or,
> they would tell us if developers can carry out (as have the patience and
> discipline) some of the strategies to success. Importantly, due to the
> sheer space of vairables that need to be controlled when humans are
> involved, a human study would not be a feasible approach to test the
> strategies thoroughly. Furthermore, it is a fantasy that a human study
> would involve more interesting benchmarks; human studies (exactly to
> control variables and lead to some conclusions) are limited to way more
> simple programs than the GTP benchmarks. 






