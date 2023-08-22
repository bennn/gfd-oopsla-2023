OOPSLA 2023
===

- Submissions due 2023-04-14
- 23 pages excluding bib
- 10pt font

<https://2023.splashcon.org/track/splash-2023-oopsla>

Revision deadline: 2023-08-13

• up to 27 pages due to added copyright notices, etc.
• should not be anonymous
• submit a detailed changelog



Followup RQs (04/23 and beyond)
---

Why do so few scenarios succeed?

- [X] where do scenarios get stuck?
  - boundary stuck at no-internal pretty often, 2/3 can make progress with profile
     395016 total
     127180 are hopeless anyway
     264075 can progress with prf (total OR self)
     3761 not hopeless, but no prf progress
     usually above the 50% mark (382551)
  - prf stuck at no-actionable
     usually above 50% typed (727262)
     [ ] TODO any more insights?
  (notes/why-stuck.txt)

- [X] t-test, significant differences
  - [X] cite Amherst etc for insignificant differences (<20ms)
    ... acquire same? forth same for sure
  - [X] use Andy Georges formulas for t-test,,, chapter 4.3?
  - [X] configs within the noise if either one holds ... stop using stddev windows!
  - [X] re-run ... results same right?

- [ ] does shallow ever help?
  - [X] for critical pairs on 1st level, rarely (few such pairs anyway), ~13 cases
    (notes/shallow-help.txt)
  - [X] 1st level, how often toggling helps?
  - [X] 2nd level, ditto
    (notes/sh
  - [ ] TODO filter scenarios
  - [ ] more levels ... show all
  - [ ] add to paper

- [ ] where in the lattice are the slow points? evenly distributed?
  - for deep only, typically low but pretty even split
    (notes/sparse-slow.txt)

- [ ] how do the results look with 1.1x as success?
      what about 2x or 3x?
  (TODO)

- [ ] what to do about adaptors? how big of a threat are they?
  - [ ] why is forth config 0200 so slow? any deep? any adaptor? any base library?
  - [ ] count num failures due to adaptors, report like #hopeless
  - [ ] 

- [ ] why black holes?!
  - [ ] take5 typed is slower, why? fix it.

- vague quesions:
  - [ ] can wrappers make garbage un-claimable in a big way?
  - [ ] is shallow blocking chez optimizations? any way to check?!

Scripts:

- code/stuck.rkt
- code/shallow-help.rkt


latexdiff
---

Run command below, then delete the "acks" from the diff file.

```
latexdiff -c ldiff.cfg --exclude-text="section,subsection,subsubsection" submission/paper.tex revision/paper.tex > diff/diff.tex
```



