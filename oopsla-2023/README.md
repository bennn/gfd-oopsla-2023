OOPSLA 2023
===

- Submissions due 2023-04-14
- 23 pages excluding bib
- 10pt font

<https://2023.splashcon.org/track/splash-2023-oopsla>



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

- [ ] does shallow ever help?
  - [X] for critical pairs on 1st level, rarely (few such pairs anyway), ~13 cases
    (notes/shallow-help.txt)
  - [ ] 1st level, how often toggling helps?
  - [ ] 2nd level, ditto

- [ ] where in the lattice are the slow points? evenly distributed?
  - for deep only, typically low but pretty even split
    (notes/sparse-slow.txt)

- [ ] how do the results look with 1.1x as success?
      what about 2x or 3x?
  (TODO)


Scripts:

- code/stuck.rkt
- code/shallow-help.rkt

