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


## Getting Started

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

Install [Racket v8.8](https://download.racket-lang.org/racket-v8.8.html) (at least)
or run the Dockerfile.

Racket way:

```
# install from https://download.racket-lang.org/racket-v8.8.html
# then install packages:
raco pkg install --auto pict-abbrevs with-cache gtp-util text-table
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
# will print many lines, compare them to: `example-output-run.sh.txt`
cd ..
```

Check that `rational-experiment/out` has 27 files.

Rerun the trail analysis script and tex-rendering script:

```
cd render
sh run.sh
# will print, compare output to: `example-output-run.sh.txt`
# you can also run mkdata.rkt and tex.rkt directly
cd ..
```

Now rebuild `main.tex` and compare `main.pdf` to `fsm.pdf`.

If you used Docker, exit before rebuilding ... or install LaTeX.

The final two figures are smaller in `fsm.pdf` because we commented the lines
in `main.tex` for the other benchmarks (not `fsm`) before building.

All done!


## More

TODO


## Takikawa

-t 3 in run.sh, both run scripts



## Rational Programmer, Resolve interfaces

getint.rkt
 See interface-for/ for examples

run.sh ~10 seconds
default is fsm all strategies

gotta download to get all

use -t flag for Takikawa, -s flag for strategy

3 per strategy, for example ...
  out/fsm-con-boundary.rktd
  out/fsm-con-prf_self.rktd
  out/fsm-con-prf_total.rktd


## render

mkdata.rkt => figure-data
 depends on
 ../rational-experiment/out

tex.rkt => ../tex

