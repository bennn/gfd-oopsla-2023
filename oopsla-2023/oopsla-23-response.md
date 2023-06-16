Thank you all for the constructive feedback.

This response follows the format suggested by the PC chair:

* OVERVIEW
* CHANGE LIST
* RESPONSE


## OVERVIEW

_A short overview of the response_

This part of the response addresses three major points:

 1. Concerns about limited generality (Reviews A and C).
 2. Incorrect statements in Review A.
 3. Concern that the rational programmer obfuscates a simple method (Review A).

### 1. Concerns about limited generality (Reviews A and C)

Review A says:

> the experiments are tailored to Racket and do not obviously generalize to
> other gradually-typed languages, and even there, they do not work that well.

and asks:

> How do you imagine your results being used?

Review C asks:

> Q1: How do these results generalize beyond the walled garden of
> gradually-typed Racket?
>
> Q2: More specifically, is "boundary profiler" really an 'approach' in the
> same sense that "statistical profilers" are, or is it a specific artifact?

At the moment, it is true that no other language comes with the necessary
ingredients to repeat this experiment (deep and shallow backends, sizeable
benchmarks, and a boundary profiler). Such things happen in research; for
example, only TypeScript has the userbase to support a large-scale corpus
study of programming with optional types. Until a second language
implements the same set of ingredients, we cannot argue on a scientific
basis that our experimental results generalize.

That said, our findings do offer lessons for language designers and
researchers at large:

- For designers implementing a language with deep types, our results show the benefits
  from also adding a boundary profiler.  (@Review C, a boundary profiler is a refinement of
  a statistical profiler, well in reach for other languages. Andersen et al.
  have an R implementation.) The results also show that there is room for
  improvement on the profiling front here --- an avenue for further research
  whose success our experimental method can help quantify. 

- For designers implementing a language with shallow types, there is a need for research on improving
  profiling tools.

- For designers considering the combination of the two, our results raise
  doubts about the benefits from the combination given the status quo in
  tooling and compiler support. More research is needed to unlock the
  potential of the combination.

- For researchers and designers that are working on gradual typing
  languages, our experiment offers a template for evaluating their own linguistic setting.

Hence, we imagine our results being used to guide pragmatic language and profiler design.


### 2. Incorrect statements in Review A

> a large chunk of configurations - almost half are hopeless to fix in the
> first place, a fact that is well-known

No prior work shows that configurations are hopeless in the sense of this
paper (unable to reach a fast configuration). It merely shows that many
configurations run slowly.

Early work on Typed Racket (popl'16, jfp'19) did search up to two
migratory steps, but "hopeless" searches any number of steps forward.
Moreover that work concerned a `2^N` lattice (no combination of deep and
shallow).

> as is the fact that for many of the programs in the GTP benchmark suite in
> Typed Racket, there simply is no reasonable path through the migration
> lattice that avoids unacceptable overheads.

There is no prior work that studies paths from performance-debugging
scenarios to configurations with acceptable performance. Indeed, it is
unclear how to conduct such an experiment that studies such paths that a
non-omnipotent developer can follow without the rational programmer
method.

Two papers (jfp'19, pldi'22) do study the worst-case overhead on all paths that
**start at the untyped configuration** and end at the typed configuration.
This is a very different question than the one we ask (whether slow
configurations can improve).

> given that you know the full lattice, you could have also compared against
> the "optimal" strategy

Figure 5 does exactly this. A configuration is hopeful if an optimal strategy
can reach a fast configuration.

> the bulk of that work was just reproducing the numbers from
> earlier papers on your own hardware

Actually, the bulk of the work was measuring the `3^N` lattice for synth,
which Greenman (pldi'22) did not measure. That work measured two `2^N` lattices
(deep and shallow separately) for this very large benchmark but not its full
`3^N` lattice. That work also did not measure `3^N` lattices for the rest of
the very large benchmarks from GTP.

We also spent significant time collecting profiling data that previous
work did not concern with. 

A separate issue is that in the last year there have been a number of
performance improvements in Typed Racket, and those change the performance
profiles of some of the benchmarks. 

### 3. Concern that the rational programmer obfuscates a simple method (Review A)

Review A says:

> the rational programmer method seemed to be a complicated way of talking
> about the most straightforward way of doing your experiments, which come down
> to: "In the migration lattice where each configuration is annotated with its
> performance relative to the fully untyped configuration, for each
> configuration where the performance is worse than some threshold, does the
> output of profiler X indicate a (path to a) reachable "next" configuration
> whose performance is either better than the threshold or at least better than
> right now?". Talking about a rational programmer and satisficing behavior and
> such seems to obfuscate more than illuminating anything.

This argument skims over the key question that the rational programmer lets us
answer: **how to choose a "next" configuration**. Profilers do not propose a
next step, they simply measure. People in a user study might follow hunches
instead of the profiler output. With the rational programmer method, we can
systematically test interpretations of profiler output,
in a large and reproducible experiment.

Satisficing is important because human programmers do not have the luxury
of seeking optimal paths. They cannot explore the full lattice; they must
make do with limited means.


## CHANGE LIST

_A list of the changes that you plan to make in response to the reviews along
with an estimation of the time you think the implementation of the changes will
take._

We plan to make five sizeable changes:

- spell out general takeaways in the conclusion
- provide more background on gradual typing, checking strategies, and the
  implementation-dependent costs of shallow checks (as outlined in the
  point-by-point responses further on)
- fix the formalism mistake in section 4.2 (thanks Review C)
- give more detail about the experimental setup (thanks Review B)
- clearly compare to prior work on Typed Racket performance (thanks Review A)

We also plan to fix other issues noted in the reviews. See the point-by-point
response below for more details.

The changes will take at most one week to implement.


## RESPONSE

_A reviewer-by-reviewer list of answers to questions. Please label the answers
in a way that makes it easy for the reviewers to find answers to the
questions/issues they raised._

The response below interleaves text from the reviews (in the order that it
appears in the review) and our replies.


### Review #201A

> Maybe the fact that this experimental setup matches the scheme of the rational
> programmer provides evidence for its usefulness, but it seems to me that that
> should be much more of a side-note in either a paper about the method itself or
> in the related work section here, but the paper seems to present this as much
> more novel than it is.

On the contrary, it was the rational programmer method that provided the
template and tools for designing the experiment. Prior work contemplated
migration questions (popl'16, jfp'19) but lacked a compelling way to answer them.
This work managed to do it by instantiating the rational programmer.

At the meta level, the paper is the fourth in a series that explores the
idea of studying language pragmatics with the rational programmer. The
rational programmer is the first attempt for creating a general tool for
such studies. The previous three papers look at debugging type-level and logical
mistakes, while this is the first that looks into improving performance issues
with profilers.  As such, this paper is at least as much a study of
the rational programmer method as it is a study of profilers in gradual typing.

The review seems to deny that there is value in exploring the
rational programmer method across different aspects of language
pragmatics. Why?

> > At the object level, the results of the rational programmer experiment
> > provide guidance to developers about how to best use the feedback from
> > profilers during type migration.
> 
> While technically true, this quite a weak statement. Said guidance comes down
> to "always add deep types to the less-typed side of the highest-overhead
> boundary that is found by the boundary profiler", because that leads to success
> in about half of the cases explored here.

We were surprised by this. Based on Greenman's work (pldi'22) and the
work on Reticulated Python, we fully expected shallow types and the
statistical profiler to help in the lower half of the lattice. We will
clarify in the paper.

> The bigger meaning implicit in the paper is that using a profiler in this way
> is actually a useful strategy, which is not really borne out because of the
> fact that a large chunk of configurations - almost half are hopeless to fix
> in the first place, a fact that is well-known, as is the fact that for many
> of the programs in the GTP benchmark suite in Typed Racket, there simply is
> no reasonable path through the migration lattice that avoids unacceptable
> overheads.  As such, the experiment was in some sense doomed from the start.

These "facts" are not true. See overview above.

> However, the choice of the GTP benchmark suite is a bit odd, since there it
> has already been established that reasonable paths often do not exist.

The GTP benchmark suite is appropriate as it is the largest collection of
programs that support a full gradual lattice. Furthermore, these are not
micro-benchmarks, but a collection of realistic applications written in
different styles, by different people, and for different domains. 

Finally, as we expand on in the overview, prior work studies only paths
that start at the untyped configuration and stay within a threshold of
performance. So, it does not establish the lack of reasonable paths for
resolving performance-debugging scenarios.

> Maybe one other way of looking at my criticism here is to ask: "Who is helped
> by this?" - your introduction makes it partially sound like this is a useful
> tool for actual programmers. In that case, an actual user-study might have
> been a better fit; as it would have gotten you more realistic benchmarks to
> work with, and, well, actual users. In that sense, I actually disagree with
> the usefulness of the "rational programmer" method here.

User studies do not answer the same questions as the rational programmer. They
could tell us what strategies users choose to interpret profiler output, or
whether a user can follow a specified strategy. They are not suited to
test the overall effectiveness of a strategy because of the limited patience
of humans and the huge number of confounding variables involved. By contrast,
the rational programmer can test the effectiveness of a strategy and thereby
recommend it (or not) to users.

Furthermore, the history of user studies goes against the review's claim
that a user study could lead to more realistic benchmarks. If anything, to
keep things meaningful, user studies tend to simplify the tasks or the
number of users or both.

> On the other hand, in my interpretation, the GTP benchmark suite is addressed
> at language designers and implementers, demonstrating particular pitfalls and
> areas for improvement in gradual typing systems. As someone from that corner,
> my main takeaway from this paper are a few more hard numbers on the
> unsurprising fact that there are many low-performing configurations that are
> hard to get out of.

Prior work says little about getting out of slow configurations. Moreover,
the GTP benchmarks are not a collection of pathological cases --- they are
a diverse collection of applications that aim to capture the full
experience of programming in Typed Racket. 

> To really be helpful here, a little more exploration might have helped, to
> see how far we really are away from any sort of findable reasonable path
> (also, given that you know the full lattice, you could have also compared
> against the "optimal" strategy),

We did that. Figure 5 compares to the optimal strategy.

> and what other criteria than the more or less randomly chosen 3x overhead for
> success and 1x overhead hopefulness there could be (this is particularly
> noticeable in the `take5` benchmark, which Greenman reports as getting to a
> worst shallow overhead of 2.99x, and a worst combined overhead of 2.97x;
> similarly, `dungeon` has a worst-case overhead of 15000x in with deep types,
> 4.97x with only shallow types, and 3.16x with a combination of the two; again
> close to the boundary; finally, `mbta` has worst-case overheads below 2x all
> the time, though your graphs are still empty because they are all deemed
> hopeless).

The overwhelming success of TypeScript and underwhelming success of sound gradual
typing suggests that nobody will put up with any slowdown for production (except maybe gradual
typing researchers).

Still, if you propose a limit other than 1x we will build a second set of figures
for the appendix. We have the full measurement data and scripts ready to get
that quickly.

> As you report, running all the benchmarks fully takes quite a lot of time, but
> it seems to me that the bulk of that work was just reproducing the numbers from
> earlier papers on your own hardware. I admit that the raw numbers do not seem
> to be easily available online, but did you contact the authors of the earlier
> evaluation papers to ask them if they could share those with you? That would
> have saved you months of computation time that you could have spent on just
> running the profilers on interesting configurations (those that constituted
> performance-debugging-scenarios in the original data), and thus enabled you to
> also do this for the larger ones (gregor, quadT, quadU).

As we note above in the overview, the bulk of performance data is new. (and the
profiler data is of course all new.)

There are 11,160,261 configurations across gregor, quadT, and quadU. Even
if we ignored all the configurations measured for the paper (116,154
total), we would still need more machines and more researchers to measure
those three largest benchmarks.

> I know that there's the possibility that your measurements might be
> significantly different from those of earlier publications, but in order for
> them to be valid experiments without running each benchmark on multiple
> different machines, we already have to believe that that is not going to
> happen.

If we reused older data, we would need to use an older version of Racket
(missing out on improvements to Typed Racket) for a fair comparison.

> Even if exploring the larger benchmarks was computationally infeasible, they
> might still have yielded an interesting test case for some random exploration:
> measure the fully untyped configuration. Then pick some random subset of the
> configurations, including the fully untyped one. Now, for each of them, pick a
> random migration until you get into a performance debugging scenario, then
> apply your method for addressing it. How far do you get?

Obtaining sufficiently large samples that have have statistical
significance for the full lattices is quite tricky. But thank you, this is
a good suggestion. We will propose it as future work and attribute it to the
oopsla'23 reviewers.

> Questions for author response
> -----------------------------
> - You claim that a lesson for language designers is that "the addition of
>   shallow type enforcement does not seem to help with the navigation of the
>   migration lattice" (lines 842-844), but as far as I can tell, some benchmarks
>   that do seem to show significant improvements in Greenman's data (`take5`,
>   `mbta`, `dungeon`) have been effectively excluded from your experiment
>   because of your definition of "hopelessness". How does that square with your
>   above claim?

Nobody except gradual typing researchers will put up with any slowdown for long.
Hence we set 1x overhead as the criteria for success.

If you propose a looser overhead limit, we will build a set of figures for the
appendix.


> - Related to the above, how does your definition of hopelessness actually work?
>   As far as I understand it, you look at the lattice to see if there is even
>   any future configuration (within some distance?) that has less than 1x
>   overhead, which excludes programs where there always is overhead for some
>   reason. How does this apply to `take5`, `mbta`, and `dungen`, which seem to
>   have reasonable worst-case overheads?

Hopelessness looks forward to any configuration, no matter the distance.

We say that any eventual overhead (>1x after migration) is unacceptable
for working programmers, thus take5, mbta, and dungeon are hopeless.


> - You acknowledge the threat to validity that you may not have covered all
>   possible strategies, but claim that in response to this threat you designed
>   your strategies "carefully". I can easily imagine combinations of strategies
>   working better and having some other heuristic to decide between them, though
>   admittedly your choice of benchmarks would make it hard to be successful for
>   any of them. Why did you not compare to an "optimal" strategy that just looks
>   at reachability in the lattice, and what exactly did being "careful" in your
>   design entail?

We considered a space of strategies that revolve around a few specific knobs
(profiler kind / optimistic-conservative), a few composite strategies that use
some knowledge of the configuration, and two baselines. The design of this
space derives from the following:

 1. developers usually pick a profiler and stick to it, rather than running
    several profilers
 2. the optimistic-conservative dimension isolates the effect of the two
    semantics
 3. the composite strategies check the effect of switching between semantics
 4. the baselines isolate the effect of the profiler and create a point of
    comparison with prior work

There will always be more alternatives and heuristics beyond these systematically
chosen ones, but the results help us gauge how other alternatives would fare.
For instance, combining statistical and boundary profiling seems unlikely to
succeed based on the poor success of statistical.

> - Is there something about the "rational programmer" experiment that is not
>   obvious and that I missed?

See overview and comments above.

> - How do you imagine your results being used?

See overview above.


### Review #201B

> section 8 concludes: "Onward! Onward! ever Onward! IBM!"

That anthem was new to us. Thanks :)

> line 24 and many places elsewhere - there's an assumption in the
> writing that e.g. shallow or transient checks will always slow down
> a program linearly. Blowing my students' trumperts, Roberts et al 2016
> shows that's shouldn't be the case for  jitted implementation.

Right, we will add a more nuanced discussion.

> reflecting on that point - there are also complementary implementation
> assumptions coming through in the "lessons' section. what I mean here
> is he assumption that typed code is always going to be faster than
> untyped code. That's obviously the case in Racket (either version, I
> guess you're running on top of Chez, probably worth mentioning) but
> not necessarily the case. So again, unfortunately things are more
> implementation dependent than we would like.

Yes, we need to contextualize the lessons and say more about the experimental
setup.

> a few short!  very short!  code examples showing deep vs shallow type
> checks might help.  Also how modules set their type checking policy?

Yes!

> 111 be good to know module granularity earlier in the intro

Yes

> and then towards the end, I'd be really interested to know any
> thoughts or even opinions about granularity questions.  If a utility
> module has a hundred strihg functions, but an application uses only
> one ("leftpad", say) doesn't that make it more expensive to fix if
> that module isn't typed - because you'd have to type every function in
> the module?  alternatively could you refactor the module or even put a
> typed version of leftpad ihto a separate module, type that one, then
> bind the rest of the code to use just that one?

Indeed it would be expensive. Either the whole module needs types,
or leftpad needs to move to its own module (a tool might help).

Given the poor results for module-level gradual typing, we expect worse results
for fine-grained languages --- so many places to change, so likely for a small
change to add costs. We will elaborate in the paper.

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

That would help, partly. We also need to know how often a deep check
(and its subchecks) get executed. Your next point below about counting
would help.

> could the boundary profiler just count the number of crossings, rather
> than the time?

No, because the boundary profiler is sample-based just like the statistical
profiler. But one could imagine a different tool ... the pop1'16 authors
did something like that using contracts.

> 216 often faster, so sometime it isn't :-)
> 
> Fig2 not clear the terms "conservative" and "optimistic" help here.
> isn't "optimistic" just deep and "conservative" shallow?

Those words arose from our programmer-oriented design method for
strategies --- optimistic denotes here a choice that goes for eliminating 
a boundary putting aside worries for ripple effects; conservative denotes the
opposite. We will reconsider them.

> 333 rational programmers  -- but rational programmers aren't
> programmers, they're *not* people!  could reword to avoid that
> implication.  Yes, I'm being really picky.  yes that's because there's
> an implcit bias to longer reviews.

Yes, rewording is a good suggestion.

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

This is worth a discussion point. We sidestep the issue by using
the GTP benchmarks --- someone already found types for them.

> 550 cite / expand GPT benchmarks &^ justify the choice

Yes

> 584 WOW!  that's a serious benchmarking effort!! 
> 
> howdo you know you got "reliable performance measures" on the cloud?
> did you test for that?
> how do you know you're out of VM warmup effects?
> (see Edd Barrett et al, OOPSLA 2017).

This cloud let us reserve specific nodes. We checked the measurements
for instability and found nothing suspicious. Our main defense against
warmup is that (all but a few) configurations take +1 seconds to run.
We will clarify these points in the paper.

> If I take 561-564 at face value, it seems you did 11 runs; it seems
> there's an assumption neither boundary nor statistical profiler would
> interact with the VM or optimiser; simimlarly your measurements are
> just one run each for stat and boundary profiler for each
> configuraiton?   Is that right?  if not, fix it!
> if so, can you justify it?   why not do 1 startup run,
> 4 genreal runs, three runs with each profiler? 

We assume that VM / optimizer effects are not enough to change the
relative order of profile points, yes. Three profile runs would be
good, but expensive (in time and disk space). We will mention
this as a threat to validity.

> I thought Fig9 was a misprint.  Cute, but perhaps always put figures
> after the first mention in the tet

Will rearrange.

> Questions for author response
> -----------------------------
> 
> * does boundary profiling really line up with deep types,
> and shallow profiling really line up with shallow types?
> Not sure why I got that impression early on, but I did.

No. We will reword to try and avoid that impression.

> * OK there is the "leftpad" question below.

See answer above.

> * and this one: how can programmers tell if their slowdown is caused by
>   type-checking vs by a slow algorithm. Perhaps I wrote an O(N^2)
>   leftpad?  e.g. at line 388 - reseachers care, but why would
>   programmers care whether the slowness is caused by their code or the
>   typechecking?   Would e.g. an op[tion to *turn off all checking* at
>   the module level - e.g. treat migratory types as if they were
>   optionala* help answer this question?
>   is that answer implementation dependent?

Yes, optionals would help. Typed Racket has a #lang for that:
`typed/racket/no-check`. It also recently got `typed/racket/optional`, which
runs the typechecker (nocheck treats types as comments) and interoperates
better with sound types.

> * how accurate is this cynical take: typed racket generates the best
>   code when fully ("deeply") typed and gradual typing imposes costs at
>   module boundaries. Therefore: to speed up code, use the boundary
>   profiler, and make things deeply typed.

Figure 4 supports this cynical take.


### Review #201C

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

Thank you for the comments. Section 2 assumes too much background knowledge; we
will improve it.

Short explanation: adding deep types to one module improves performance
through type-driven optimizations but lowers performance at boundaries to
non-deep modules. If all modules are deep and there are no boundaries,
there is no cost and only potential improvements.

> Section 4.2 – "the 3**n configurations of [the lattice] are ordered: Pi < Pj
> iff Pj has at least one component that is untyped in Pj."  Putting aside what
> appears to be a typo, this still does not appear to say what I think the
> authors intend.  I think you probably mean something like Pi < Pj iff for all
> components c, if c is typed in Pi it is typed in Pj, AND there exists some
> component c' that is typed in Pj but not in Pi.  Otherwise, I am confused.

You are correct, we will fix this.

> Questions for author response
> -----------------------------
> Q1: How do these results generalize beyond the walled garden of gradually-typed
> Racket?  

See overview above.

> Q2: More specifically, is "boundary profiler" really an 'approach' in the same
> sense that "statistical profilers" are, or is it a specific artifact?

See overview above.

To be precise, a boundary profile samples stack traces and groups them
by the most-recent boundary on the stack (dropping traces that are not
enclosed by a boundary). Finding a way to mark and unmark the stack
is the main technical challenge to implementing a boundary profiler.


