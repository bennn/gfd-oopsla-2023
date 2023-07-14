Artifact for OOPSLA'23 paper:

 _How Profilers Can Navigate Type Migration_

### Contents:

- `artifact.tar.gz` 
  code for reproducing our rational programmer experiment
- `benchmarks.tar.gz`
  GTP Benchmarks without and with modifications
- `cloudlab.tar.gz`
  for measuring performance on CloudLab
- `figure-data.tar.gz`
  figures and summarized data for the paper
- `rational-trails.tar.gz`
  output from the rational programmer
- `raw-data.tar.gz`
  running times, boundary profile output, and statistical profile output for all benchmarks

OOPSLA AEC: for artifact evaluation, start with `artifact.tar.gz`.
Ignore the other datasets. Follow instructions in `artifact/README.md`.


### How it fits together:

1. The GTP Benchmarks are the starting point. We investigated profiler-based
   navigation on them.
2. For every configuration of most benchmarks (see paper), we used CloudLab
   to collect raw performance data: running times & profiler output.
3. Our Rational Programmer used the performance data to navigate lattices.  It
   created one trail for every benchmark configuration.
4. We created several figures to analyze the trail data.

The artifact has code for running the rational programmer experiment,
generating figures, and rendering a PDF.


### What's in the data?

- `artifact.tar.gz`
  * `rational-experiment/` code for running the rational programmer, sample output
  * `render/` code for analyzing rational programmer output and making figures
  * `tex/` output from the rendering script
  * `main.tex` source for a PDF that rebuilds the main figures from the paper
  * `Makefile` build script for `main.tex`
  * `all.pdf` example output based on all benchmarks
  * `fsm.pdf` example output based on the `fsm` benchmark alone

- `benchmarks.tar.gz`
  * `gtp-bench/` GTP Benchmarks v7.0
  * `bnd-bench/` a few benchmarks that we modified to help the boundary profiler
    deal with adaptor modules (see paper, appendix A)

- `cloudlab.tar.gz`
  * `profile.py` a CloudLab script that makes an experiment profile (see CloudLab docs)
  * `rds-cloudlab.tar.gz` initial code for the CloudLab profile, which contains source
    code for the Racket programming language (the `profile.py` script should
    download a copy of this archive from Brown University)
  * `install_boundary.sh` run this to enable boundary profiling
  * `install_statistical.sh` run this to enable statistical profiling
  * `navigation-extra.tar.gz` support code for the install scripts
    + contains a script `bmg.rkt` for running profilers
  * `output.tar.gz` a blank gtp-measure output directory 

- `figure-data.tar.gz`
  * output from our analysis of the Rational Programmer trails
  * copy to `artifact/render/figure-data/`, then run `artifact/render/tex.rkt`,
    and finally run `artifact/Makefile` to rebuild the main figures from the
    paper using our figure data

- `rational-trails.tar.gz`
  * output from the Rational Programmer
  * copy to `artifact/rational-experiment/out/`, then run `artifact/render/mkdata.rkt`
    to regenerate figure data from our rational programmer output (then run `tex.rkt`
    and `Makefile` to rebuild figures)

- `raw-data.tar.gz` (beware, 5GB unpacked)
  * `runtime/` running times collected by gtp-measure
  * `profile/` statistical profiler output
  * `boundary/` boundary profiler output


### Cloudlab Instructions

Instantiating the CloudLab profile is the first step toward reproducing our
experiments.

To collect performance data, upload the benchmarks, unzip the `output.tar.gz` archive,
delete every empty file `output/**/*.out`, and run gtp-measure:

```
cd rds-cloudlab
sh run-benchmarks.sh
```

To collect (boundary / statistical) profiler data, run the matching install
script.  Then upload the benchmarks you wish to measure and find the matching
file `output/**/*.in`. Upload to your home directory.

Use the `bmg.rkt` script to collect. Example:

```
cd rds-cloudlab

./racket-8.6.0.2/bin/racket bmg.rkt -b ../fsm ../fsm.*.in
# b = boundary profile

./racket-8.6.0.2/bin/racket bmg.rkt -p ../fsm ../fsm.*.in
# p = statistical profile
```

