# artifact

Artifact for OOPSLA'23 paper `#201`:

 _How Profilers Can Help Navigate Type Migration_

Goals:

- provide all the data that we collected
- show how to re-run the rational programmer experiment
- build figures in the style of the paper

Relative to the paper, this artifact focuses on Section 5.2 and onward.

This artifact does not focus on Section 5.1 (collecting performance data
on CloudLab) because it is expensive to recollect. The full experiment took
thousands of CPU hours and lots of manual work to reserve machines, divide
work among them, and combine the outputs. There are brief instructions on
Zenodo for the main steps to reproduce.

Zenodo:

<https://doi.org/10.5281/zenodo.8136116>


## Getting Started

Download and unpack `artifact.tar.gz` from Zenodo.

Check that the artifact has the following files and directories:

```
- Dockerfile
- Makefile
- README.md
- all.pdf
- fsm.pdf
- main.tex
- rational-experiment/
- render/
- tex/
```

Run `git init; git add .; git commit -m "init"`.
Later on, if you make changes, you can use git to revert them.

Build `main.tex`. Compare `main.pdf` to `all.pdf` and to the submitted paper.
(If you have `pdflatex` installed, run `make`.)

Now delete all the data for `fsm`:

```
rm rational-experiment/out/*
rm render/figure-data/*
rm tex/*.*
rm tex/h2h/fsm.pdf
rm tex/sky/fsm-feasible.pdf
```

At this point, `main.tex` should not build. Confirm that (e.g. by running `make`).

(You are welcome to delete everything under the `tex/` folder, but then
 you must edit the final two figures in `main.tex` --- either comment
 everything out, or comment the lines for benchmarks other than `fsm`.)

Install [Racket v8.8](https://download.racket-lang.org/racket-v8.8.html)
or run the Dockerfile.

Racket way:

```
# install from https://download.racket-lang.org/racket-v8.8.html
# then install packages:
raco pkg install --auto lang-file-lib lang-file ppict basedir rackunit-abbrevs pict-abbrevs with-cache gtp-util text-table
  
```

Docker way:

```
docker build -t gfd-oopsla-2023 .
docker run -v "$PWD:/vol" -w /vol -ti gfd-oopsla-2023 bash
```

Rerun the rational programmer experiment:

```
cd rational-experiment
sh run.sh
# will print many lines similar to: `example-output-run.sh.txt`
# expected time: 10 seconds
cd ..
```

Check that `rational-experiment/out` has 27 files.

Rerun the trail analysis script and tex-rendering script:

```
cd render
sh run.sh
# compare output to: `example-output-run.sh.txt`
# you can also run mkdata.rkt and tex.rkt directly
# expected time: 5 seconds
cd ..
```

Now rebuild `main.tex` and compare `main.pdf` to `fsm.pdf`.

(If you used Docker, exit before rebuilding ... or install LaTeX in the
Docker.)

Unless you edit `main.tex` to show only the fsm plots in the final two
figures, those figures will be larger in your `main.pdf` than in `fsm.pdf`.
Compare only the fsm subfigures in each.

All done!


## More

There is more that can be done with the artifact and Zenodo data.


### Zenodo

Zenodo has all the data that we collected. You can download and use this data.

Two items are easy to use:

- `figure-data.tar.gz` is a drop-in replacement for `render/figure-data/`.
  Download, unpack, run `tex.rkt`, and rebuild `main.tex`.
- `rational-trails.tar.gz` can replace `rational-experiment/out/`. Download,
  unpack, then re-render (`cd ../render; sh run.sh; cd ..; pdflatex main.tex`).

More ambitious:

- Both `benchmarks.tar.gz` and `raw-data.tar.gz` are required to run the rational
  programmer on a benchmark. Unpack both. Copy the benchmark source to
  `rational-experiment/gtp-bench`. Copy the runtime, boundary, and statistical
  profile data for that benchmark to the folders under `rational-experiment/data`.
  Add the benchmark's name to the list in `rational-experiment/run.sh` (next
  to fsm). Lastly, run `run.sh` to generate trail output.

Finally:

- `cloudlab.tar.gz` has scripts for collecting performance data.  The readme on
  Zenodo outlines how to use them. Collecting the data took us several months.


## How to change the success criteria

The paper has a strict success criteria: 1x overhead.

To see results for a weaker criteria, edit `render/run.sh`. Change `-t 1` to
`-t 3` or another number. Then run and rebuild the main pdf.

To use a stronger criteria, edit `rational-experiment/run.sh` and
`render/run.sh` to use a different `-t` value. Then rerun both in order.

(The rational programmer needs to rerun for a stronger criteria because
it might have stopped short with the weaker one. Suppose we strengthen from 1x
to 1/2x. Before, the rational programmer would stop at a 1x configuration.
After, it needs to keep searching to improve that 1x to a 1/2x configuration.)


## How to add a strategy

The file `rational-experiment/main.rkt` implements six strategies from
the paper:

- optimistic (known as "opt" in the code)
- cost-aware optimistic (cost-opt)
- conservative (con)
- cost-aware conservative (cost-con)
- configuration aware (limit-con)
- null (randomB)

You can add a new strategy by writing a function with the type:

```
 ConfigurationID -> [Values StatusCode ConfigurationID]
```

where:

- `ConfigurationID` is a string made of "0" "1" or "2" characters
  - the length of the string equals the number of modules in a benchmark
  - 0 = untyped, 1 = deep typed, 2 = shallow typed
- `[Values A B]` means the function returns two items
- `StatusCode` is one of:
  - `(list 'success Reason)`
  - `(list 'error Reason)`
- `Reason` is any symbol, for example `'unknown`

For example, this function moves to the untyped configuration. It fails if the
input is already untyped:

```
(define (untyped-strategy cfg)
  (define u-cfg (regexp-replace* #rx"1|2" cfg "0"))
  (define status
    (if (equal? cfg u-cfg)
      (list 'error 'already-untyped)
      (list 'success 'removed-all-types)))
  (values status u-cfg))
```

To run the rational programmer with this strategy:

1. Add it to the file `rational-experiment/main.rkt`
2. Add a name to the list `strategy*` near the top of the file.
3. Add a case to the function `run-profile` that uses your function and name,
   like this:
   `((untyped-strategy)
     (lambda (bm-name perf-info profile-dir #:P [profile-mode #f]) untyped-strategy))`
4. Add a similar case to the function `run-boundary`:
   `((untyped-strategy)
     (lambda (bm-name perf-info benchmark-dir) untyped-strategy))`

Finally, rerun `rational-experiment/run.sh`. The directory
`rational-experiment/out/` should have 3 additional files from running your
strategy with the boundary profiler, statistical profiler (self), and statistical
profiler (total). If you used `untyped-strategy` from above, All three files will
have the same results.

To implement a smarter strategy than "make everything untyped", look to the code
for examples.

After running, the next step is to render the results:

1. Edit `render/base.rkt`:
  - Add your strategy name to the list `all-strategy-name*`
  - Add the output filenames to the list `all-sm-name*`. (The order of this
    determines the left-to-right order in the plots.)
2. Edit `render/tex.rkt`:
  - Add a case to the function `bg-strategy->cd-strategy`:
    Example: `(("untyped-strategy") "un-strat")`
  - Add a matching element to the list `cd-strategy*`.
    Example: `"un-strat"`.

Rerun `render/run.sh` and rebuild the main PDF.

Every plot should now include your strategy.


## How to add a benchmark

The high-level process for adding a benchmark is:

1. Write code that supports a full 3D lattice, like the GTP Benchmarks do.
2. Modify any adaptor modules to match the style of `bnd-bench/` code
   rather than `gtp-bench/` code.
3. Collect runtime & profiler data for every configuration
4. Tell the rational programmer how to interpret boundaries:
   - Create a file `rational-experiment/interface-for/BENCHMARK` that maps
     variable names to module names
   - The instructions in `rational-experiment/getint.rkt` can help.

This is a difficult process.

