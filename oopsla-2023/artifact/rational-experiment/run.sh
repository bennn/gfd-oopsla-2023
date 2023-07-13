# Script to fire off a rational programmer experiment

# To change the success criteria from 1x to something looser:
# - add the `-t` flag (example `-t 2` for 2x)

# To run one strategy instead of all strategies:
# - add the `-s` flag
# - options are:
#   * opt
#   * con
#   * cost-opt
#   * cost-con
#   * limit-con (known as "config aware" in the paper)
#   * randomB

# To run other benchmarks:
# - download data to:
#   * ./data/runtime
#   * ./data/boundary
#   * ./data/profile
# - download benchmark source to ./gtp-bench

# for BB in acquire dungeon forth fsm fsmoo jpeg kcfa lnm mbta morsecode sieve snake suffixtree synth take5 tetris zombie ; do

for BB in fsm ; do
  raco make -v main.rkt && racket main.rkt -s all ${BB} ;
done
