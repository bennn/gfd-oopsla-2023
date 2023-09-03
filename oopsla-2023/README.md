OOPSLA 2023
===

<https://2023.splashcon.org/track/splash-2023-oopsla>

- Submissions due 2023-04-14
- 23 pages excluding bib
- 10pt font

Revision deadline: 2023-08-13

• up to 27 pages due to added copyright notices, etc.
• should not be anonymous
• submit a detailed changelog

camera-ready deadline: 2023-09-08 ?


Followup RQs (04/23 and beyond)
---

Why do so few scenarios succeed?

- [X] where do scenarios get stuck?
  - boundary stuck at no-internal pretty often, 2/3 can make progress with profile
     395016 total
     127180 are hopeless anyway
     264075 can progress with prf (total OR self)
     3761 not hopeless, but no prf progress
     usually above the 50% mark (382551) => obvious, typed to untyped
  - prf stuck at no-actionable 745673 times
     overall usually above 50% typed (727262 out of 745753)
  (notes/why-stuck.txt)

- [X] does shallow ever help?
  - [X] for critical pairs on 1st level, rarely (few such pairs anyway), ~13 cases
    (notes/shallow-help.txt)
  - [X] 1st level, how often toggling helps?
  - [X] 2nd level, ditto
  - win for 679 / 113183 = 0.60% overall by toggling all deep to all shallow
    - low better 297 / 679 = 43.74
      ... not great! perhaps <= would be better?  ... oh, no, less-equal gives 44.92, not much better!
  - improve for 73985 / 113183 = 65.37% by ditto ^^^
  (notes/shallow-help.txt)

- [ ] can we find a 100% success strategy? if not why???
- [ ] does the starting point matter for result? stratify by 2x, 10x, 50x scenario
- [ ] what config stuck at? (hash config count) ... get size + distribution
- [ ] quadT, database experiment
- [ ] lnm, can we find a bug in the handwritten plot library types? would be a nice story, but I think, unlikely!!!

- vague quesions:
  - [ ] can wrappers make garbage un-claimable in a big way?
  - [ ] is shallow blocking chez optimizations? any way to check?!


latexdiff
---

Run command below, then delete the "acks" from the diff file.

```
latexdiff -c ldiff.cfg --exclude-text="section,subsection,subsubsection" submission/paper.tex revision/paper.tex > diff/diff.tex
```



