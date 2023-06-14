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


# Ben's Notes

-----------------------------------------------------------------------------

> Review #201A
> C. Weak reject
> X. Expert
> 
> Paper summary
> -------------
> This paper is concerned with plausible migration paths for migratory typing. As
> individual configurations of mixed type annotations in gradually-typed programs
> can experience unacceptably high performance overhead, it is an important
> question whether there are reasonable paths to migrate a program from a less
> typed to a more typed configuration via intermediate steps with acceptable
> overheads, and if so, how to find such paths, as at many points, a programmer
> has many choices of where to focus there efforts for adding type annotations.
> The authors propose using profilers, and different kinds of them, to indicate
> which part of a program should be annotated next. They investigate this idea
> for the gradual typing performance benchmark suite assembled for (Typed)
> Racket, which increases the search-space by allowing to toggle type-checks
> between "shallow" and "deep" modes, but makes it (mostly) tractable by only
> toggling types and their modes on whole modules. The end result in the
> particular setup chosen by the authors is that only the boundary-check profiler
> seems to be able to provide helpful guidance in a significant number of cases,
> if at all, and using "shallow" types almost never leads to a performance
> improvement.
> 
> Assessment
> ----------
> ### Pros
> + Important Problem
> + Accessible
> 
> ### Cons
> - Limited Generality
> - Inadequate exploration of even the limited scope
> - The talk about the "rational programmer" method obfuscates a rather simple
>   methodology
> 
> I want to begin by stressing that I agree with the authors that finding smooth
> migration paths in gradually-typed programs is an important problem, and tools
> and techniques that guide programmers through this process would be useful.
> However, as I understand the work presented in this paper, it does not provide
> a significant improvement over the state of the art or provide enough
> significant insights.
> 
> The paper claims two contributions in particular, which I cite and address in
> reverse order:
> 
> > At the meta level, the application of the rational programmer method to the
> > performance problems of type migration provides one more piece of evidence
> > about its usefulness.
> 
> This may be a mismatch between different subcommunities, but as someone who is
> interested in the design and implementation of gradually-typed languages, the
> rational programmer method seemed to be a complicated way of talking about the
> most straightforward way of doing your experiments, which come down to: "In the
> migration lattice where each configuration is annotated with its performance
> relative to the fully untyped configuration, for each configuration where the
> performance is worse than some threshold, does the output of profiler X
> indicate a (path to a) reachable "next" configuration whose performance is
> either better than the threshold or at least better than right now?". Talking
> about a rational programmer and satisficing behavior and such seems to
> obfuscate more than illuminating anything.
> 
> Maybe the fact that this experimental setup matches the scheme of the rational
> programmer provides evidence for its usefulness, but it seems to me that that
> should be much more of a side-note in either a paper about the method itself or
> in the related work section here, but the paper seems to present this as much
> more novel than it is.

This summary glosses over the critical question of **how to choose a next
configuration**. 

TODO

> > At the object level, the results of the rational programmer experiment
> > provide guidance to developers about how to best use the feedback from
> > profilers during type migration.
> 
> While technically true, this quite a weak statement. Said guidance comes down
> to "always add deep types to the less-typed side of the highest-overhead
> boundary that is found by the boundary profiler", because that leads to success
> in about half of the cases explored here.

It is very surprising that Shallow does not help in the lower half of the lattice.
We fully expected it to give incremental improvements

> The bigger meaning implicit in the
> paper is that using a profiler in this way is actually a useful strategy, which
> is not really borne out because of the fact that a large chunk of
> configurations - almost half are hopeless to fix in the first place, a fact
> that is well-known, as is the fact that for many of the programs in the GTP
> benchmark suite in Typed Racket, there simply is no reasonable path through the
> migration lattice that avoids unacceptable overheads.

Prior work says little about migration paths. While it is well-known that many
configurations have high overhead, it was (until now) unclear whether
configurations with high overhead could find a path "down the mountain" to
something reasonable.

Early work on Typed Racket looked one or two steps ahead for a fast
configuration (Takikawa et al. POPL'16; Greenman et al. JFP'19).
Those results are both uninformative and unactionable compared to ours:

 1. Uninformative, because they reject incremental paths such as
    30x -> 20x -> 10x -> 1x.

 2. Unactionable, because they try all possible next steps to find
    the optimal one. This is not a viable strategy for human programmers.

Greenman PLDI'22 studies paths that **start untyped** and end up fully-typed.
Whether these paths have any bottlenecks is an entirely different question
than how to fix a bottleneck once it arises.

> As such, the experiment
> was in some sense doomed from the start. There are several smaller issues with
> the overall experimental setup and design choices that I will expand upon more
> in the comments for authors, but the last key bit is that as presented, the
> experiments are tailored to Racket and do not obviously generalize to other
> gradually-typed languages, and even there, they do not work that well.
> 
> Comments for authors
> --------------------
> I do appreciate the focus on paths, since their existence and an ability to
> find them seems much more important in practice than the overall distribution
> of acceptable-performance configurations that the original gradual typing
> performance evaluation work focuses on. However, the choice of the GTP
> benchmark suite is a bit odd, since there it has already been established that
> reasonable paths often do not exist. Maybe one other way of looking at my
> criticism here is to ask: "Who is helped by this?" - your introduction makes it
> partially sound like this is a useful tool for actual programmers. In that
> case, an actual user-study might have been a better fit; as it would have
> gotten you more realistic benchmarks to work with, and, well, actual users. In
> that sense, I actually disagree with the usefulness of the "rational
> programmer" method here. On the other hand, in my interpretation, the GTP
> benchmark suite is addressed at language designers and implementers,
> demonstrating particular pitfalls and areas for improvement in gradual typing
> systems. As someone from that corner, my main takeaway from this paper are a
> few more hard numbers on the unsurprising fact that there are many
> low-performing configurations that are hard to get out of. To really be helpful
> here, a little more exploration might have helped, to see how far we really are
> away from any sort of findable reasonable path (also, given that you know the
> full lattice, you could have also compared against the "optimal" strategy)

Hopeless = optimal

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


> As you report, running all the benchmarks fully takes quite a lot of time, but
> it seems to me that the bulk of that work was just reproducing the numbers from
> earlier papers on your own hardware. I admit that the raw numbers do not seem
> to be easily available online, but did you contact the authors of the earlier
> evaluation papers to ask them if they could share those with you? That would
> have saved you months of computation time that you could have spent on just
> running the profilers on interesting configurations (those that constituted
> performance-debugging-scenarios in the original data), and thus enabled you to
> also do this for the larger ones (gregor, quadT, quadU). I know that there's
> the possibility that your measurements might be significantly different from
> those of earlier publications, but in order for them to be valid experiments
> without running each benchmark on multiple different machines, we already have
> to believe that that is not going to happen.

True!

> Even if exploring the larger benchmarks was computationally infeasible, they
> might still have yielded an interesting test case for some random exploration:
> measure the fully untyped configuration. Then pick some random subset of the
> configurations, including the fully untyped one. Now, for each of them, pick a
> random migration until you get into a performance debugging scenario, then
> apply your method for addressing it. How far do you get?

Good idea!

> Questions for author response
> -----------------------------
> - You claim that a lesson for language designers is that "the addition of
>   shallow type enforcement does not seem to help with the navigation of the
>   migration lattice" (lines 842-844), but as far as I can tell, some benchmarks
>   that do seem to show significant improvements in Greenman's data (`take5`,
>   `mbta`, `dungeon`) have been effectively excluded from your experiment
>   because of your definition of "hopelessness". How does that square with your
>   above claim?



> - Related to the above, how does your definition of hopelessness actually work?
>   As far as I understand it, you look at the lattice to see if there is even
>   any future configuration (within some distance?) that has less than 1x
>   overhead, which excludes programs where there always is overhead for some
>   reason. How does this apply to `take5`, `mbta`, and `dungen`, which seem to
>   have reasonable worst-case overheads?

We disagree about reasonable.


> - You acknowledge the threat to validity that you may not have covered all
>   possible strategies, but claim that in response to this threat you designed
>   your strategies "carefully". I can easily imagine combinations of strategies
>   working better and having some other heuristic to decide between them, though
>   admittedly your choice of benchmarks would make it hard to be successful for
>   any of them. Why did you not compare to an "optimal" strategy that just looks
>   at reachability in the lattice, and what exactly did being "careful" in your
>   design entail?

> - Is there something about the "rational programmer" experiment that is not
>   obvious and that I missed?

> - How do you imagine your results being used?



> Review #201B
> A. Accept
> X. Expert
> 
> Paper summary
> -------------
> This paper presents a "reasonable programmer" study comparing two
> different profilers being used to guide conversion of Racket modules
> from untyped to typed. After a general introduction, section 2 talks
> about profiling, and section 3 about the "rational programmer"
> methodology. Section 4 presents the experimental design, section 5 the
> results of a massive benchmarking effort, which are discussed in
> section 6.  Section 7 covers related work, and section 8 concludes:
> "Onward! Onward! ever Onward! IBM!"
> 
> Assessment
> ----------
> I found the paper generally easy to follow and well written - I only
> had to read it once! The problem is pretty well set out, the
> methodology and experimental design clearly explained, and then the
> results are likewise presented clearly. Given that questions about how
> best to introduce gradual / migratory / option / pluggable types into
> programming languages remains an important research question; along
> with related questions of how best to implement those types, and what
> other tooling is needed, I think this paper makes a solid scientific
> contribution, so I hope to see it presented at OOPSLA.  My comments
> below should be taken more as "suggestions for improvement" to an
> already good paper - not as things that are actually wrong with this
> draft.
> 
> Comments for authors
> --------------------
> 
> 
> not sure why "functional langauges" is a keyword
> 
> line 24 and many places elsewhere - there's an assumption in the
> writing that e.g. shallow or transient checks will always slow down
> a program linearly. Blowing my students' trumperts, Roberts et al 2016
> shows that's shouldn't be the case for  jitted implementation.

Right, we need a more nuanced discussion.

> reflecting on that point - there are also complementary implementation
> assumptions coming through in the "lessons' section. what I mean here
> is he assumption that typed code is always going to be faster than
> untyped code. That's obviously the case in Racket (either version, I
> guess you're running on top of Chez, probably worth mentioning) but
> not necessarily the case. So again, unfortunately things are more
> implementation dependent than we would like.

Will clarify.

> a few short!  very short!  code examples showing deep vs shallow type
> checks might help.  Also how modules set their type checking policy?

Yes

> 111 be good to know module granularity earlier in the intro

Ok

> and then towards the end, I'd be really interested to know any
> thoughts or even opinions about granularity questions.  If a utility
> module has a hundred strihg functions, but an application uses only
> one ("leftpad", say) doesn't that make it more expensive to fix if
> that module isn't typed - because you'd have to type every function in
> the module?  alternatively could you refactor the module or even put a
> typed version of leftpad ihto a separate module, type that one, then
> bind the rest of the code to use just that one?

You'd have to separate leftpad ... annoying but straightforward.

> dunno why I thought about MLKit's extensive use or profiling for
> memory, but I did.  The issue there - I think - is that good MLKit
> performance depends on the region inference which is *implicit* in the
> source code - unless you print out the AST post region inference,
> which has *explicit* regions, or look at the profiler to find out
> what's going wrong, which again identifies *explicit* regions.
> 
> howmuch of the difficulty here is that especially "deep" typechekcs
> are implicit in Racket source.  if they were made explcit (dunno how)
> would that help? 

That would help, but only partially. We need to know where the Deep checks
appear and how often they and their sub-checks (such as the result checks for
a function type) get executed.

> could the boundary profiler just count the number of crossings, rather
> than the time?  

No, because the boundary profiler is sample-based just like the statistical
profiler. We would need a different tool to count crossings.

> 216 often faster, so sometime it isn't :-)
> 
> Fig2 not clear the terms "conservative" and "optimistic" help here.
> isn't "optimistic" just deep and "conservative" shallow?

Yes! Those words are from our "careful" programmer-focused design
of strategies.

> 333 rational programmers  -- but rational programmers aren't
> programmers, they're *not* people!  could reword to avoid that
> implication.  Yes, I'm being really picky.  yes that's because there's
> an implcit bias to longer reviews.
> 
> 
> 386 or elsewhwere - is it worth talking about the difficulty -
> especailly of the philosphy implied by the "migratory" approach, that
> programmers can just annotation types into their programs *without
> restructuring that code* - i.e. without following an implicit model of
> typed programming - writing code that can in principle be typed but
> has not been.  Again this is implementation dependent.  One example
> because I'm old (and yes, I know Racket has option types which would
> do this job) --- 30 years ago, in Self, say you wantedw to record an
> *optional integer*.  Classic dyanmic langauge would be ot put a null
> in the slot, or false, or something. Self's advice was to have a
> Boolean slot, and then a separate integer slot, separating them by
> type, befause otherwise the VM's type predictors would get
> confused. That is: write dynamically typed code as if it were typed.
> while Racket's type system is indeed a wonderous thing, there must
> surely be cases where the available types are inadequate to describe a
> program's behaviour.

Yes worth mentioning, we avoid the issue with the GTP programs.

> 550 cite / expand GPT benchmarks &^ justify the choice

Yes

> 584 WOW!  that's a serious benchmarking effort!! 
> 
> howdo you know you got "reliable performance measures" on the cloud?
> did you test for that?
> how do you know you're out of VM warmup effects?
> (see Edd Barrett et al, OOPSLA 2017).

Got dedicated node,
did check for weirdness,
don't know we're out of the warmup space.

> If I take 561-564 at face value, it seems you did 11 runs; it seems
> there's an assumption neither boundary nor statistical profiler would
> interact with the VM or optimiser; simimlarly your measurements are
> just one run each for stat and boundary profiler for each
> configuraiton?   Is that right?  if not, fix it!
> if so, can you justify it?   why not do 1 startup run,
> 4 genreal runs, three runs with each profiler? 

Correct.
How to combine the profiler runs?

> I thought Fig9 was a misprint.  Cute, but perhaps always put figures
> after the first mention in the tet
> 
> 
> To encourage accountability, I'm signing my reviews in 2023.
> For the record, I am James Noble, kjx@acm.org
> 
> Questions for author response
> -----------------------------
> 
> * I don't seem to have many questions.
> I made this one up:
> 
> * does boundary profiling really line up with deep types,
> and shallow profiling really line up with shallow types?
> Not sure why I got that impression early on, but I did.

No it doesn't, we need to reword to avoid that impression!

> * OK there is the "leftpad" question below.

You have to spin out to a new module. Painful, but a tool could handle it in the
future.

> * and this one: how can programmers tell if their slowdown is caused by
>   type-checking vs by a slow algorithm. Perhaps I wrote an O(N^2)
>   leftpad?  e.g. at line 388 - reseachers care, but why would
>   programmers care whether the slowness is caused by their code or the
>   typechecking?   Would e.g. an op[tion to *turn off all checking* at
>   the module level - e.g. treat migratory types as if they were
>   optionala* help answer this question?
>   is that answer implementation dependent?

see MF, run untyped

> * like that one, other questions are really just suggestions so I
> I'm putting the rest in the suggestions list.
> 
> * how accurate is this cynical take: typed racket generates the best
>   code when fully ("deeply") typed and gradual typing imposes costs at
>   module boundaries. Therefore: to speed up code, use the boundary
>   profiler, and make things deeply typed.

Very accurate!


> Review #201C
> B. Weak accept
> Z. Outsider
> 
> Paper summary
> -------------
> Paper 201 applies the rational agent abstraction, familiar from economics, to
> the problem of type migration in gradually-typed languages.  Given a
> partially-typed program that is performing poorly due to the overhead of
> runtime type checks, a programmer (who does not wish to undo any typing efforts
> already performed) can in many cases improve the program's performance by
> adding additional type constraints. Unfortunately, the space of choices is
> large: any untyped component is a candidate, there is more than one way to add
> type checks at runtime, and the checks interact in difficult-to-predict ways.
> The programmer has access to oracles that reveal whether a choice improved
> performance – profilers – but there is more than one kind of profiler too.  How
> should a programmer navigate the 3**n latice of partially-typed programs?
> 
> The paper sets up this model, defines the search space formally, defines a way
> to meaningfully compare two strategies, and ultimately reports on a collection
> of experiments that apply strategies to traverse the search space, given a set
> of established benchmarks. Useful rules-of-thumb are extracted.
> 
> Assessment
> ----------
> Thank you for submitting this paper to OOPSLA.  I learned a lot reading it.  I
> would like to see this paper published because it is a good piece of thinking
> and writing (modulo some minor issues I raise below regarding claims and the
> formalism). 
> 
> At the end of the day, however, I am not highly confident in my rating because,
> as an outsider to this area, I do not understand how important/impactful the
> results are.  See Q1.
> 
> Comments for authors
> --------------------
> In several places, the draft makes universal assertions about fully-typed
> programs that use all deep types.  In Section 2, starting with a running
> example of a partially-typed program (hence one whose performance is presumably
> impacted by type checking overhead at runtime), the draft asserts "adding deep
> types to every module is the option that is almost always guaranteed to solve
> the problem."  This assumption is important, because it means you can navigate
> the program lattice monotonically. Later, in Section 3, the draft says "the
> fully-typed program with deep types is often faster than its untyped
> counterpart."  This is a slightly different statement – it asserts that more
> types is the remedy not only for the performance overhead of partial typing but
> also a move towards better performance compared to any untyped program.  I am
> afraid that I am just a systems programmer and not a user of gradually-typed
> Racket, so I struggle to understand why these assertions are certainly true.
> It seems to me that adding runtime checks adds overhead, and I need help
> understanding why it sometimes doesn't.

We will explain in the paper.

In short: adding Deep types to one module improves performance through
type-driven optimizations but lowers performance at boundaries to non-Deep
modules. If all modules are Deep, there is no performance hit.


> Section 4.2 – "the 3**n configurations of [the lattice] are ordered: Pi < Pj
> iff Pj has at least one component that is untyped in Pj."  Putting aside what
> appears to be a typo, this still does not appear to say what I think the
> authors intend.  I think you probably mean something like Pi < Pj iff for all
> components c, if c is typed in Pi it is typed in Pj, AND there exists some
> component c' that is typed in Pj but not in Pi.  Otherwise, I am confused.

You are correct, we will fix.

> Although this is not the sort of evaluation that I am accustomed to reading, I
> found it very interesting.
> 
> The threats to validity and and prior work sections are simply excellent.
> 
> Questions for author response
> -----------------------------
> Q1: How do these results generalize beyond the walled garden of gradually-typed
> Racket?  

... mf comments

We have two negative lessons for other languages:

- An implementation of Deep types **must** come with a boundary profiler
  to give programmers some hope of navigating the lattice.

- Shallow types call for new profiler technology.


> Q2: More specifically, is "boundary profiler" really an 'approach' in the same
> sense that "statistical profilers" are, or is it a specific artifact?

The boundary profiler is not a one-off artifact. Andersen et al. TOPLAS'19
have implemented it for R as well as Racket.

Technically, the boundary profiler is a refinement of a statistical profiler.
Whereas the statistical profiler groups samples by the current stack frame, the
boundary profiler groups by the most-recent boundary on the stack (and discards
frames that are not enclosed by a boundary). It takes some work to let the
profiler find a "most recent boundary," but in principle any language with a
statistical profiler can implement the boundary profiler approach.


