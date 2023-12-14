## TODO List

### Necessary

- [x] Follow static link. Record the static link access.
- [x] RBP and RSP: replace all occurrence of RBP (frame pointer)
- [x] Multiplication: `imulp s` implicitly use RAX and RDX, should I backup them?
  - Yes, they are backed up.
  - [ ] But is it necessary to backup RAX and RDX for `imulp` operation?
- [ ] For register allocation: caller-save and callee-save recall. (P244)
- [x] Rewrite MunchInMemory to generate more complicated memory representations, based on [instruction.py](../scripts/lab5_test/instruction.py).
  - [x] Further more: support `Imm(r1,r2,s)` form.
    Unused. See
- [ ] The `dst` and `src` of some instructions should be considered again. Rewrite/Patch when necessary.

### Perplexed

- [ ] Will register allocation affect frame size? Should `procEntryExit3` handle it? If the frame size is expected to be increased, how to modify it?

### Optional

- [ ] Register allocation _for the tree_, integrated with Munch. (P257)
- [ ] Strength reduction: optimization for multiplication (maybe utilize the `lea` instruction).
- [ ] Unreasonable: constants should be stored in 64-bit integers. See [tree.h](../src/tiger/translate/tree.h):446. I define a type `frame::Immediate` in [assem.h](../src/tiger/codegen/assem.h).
- [ ] Inefficient if-then-else translation. See P165 on the textbook. Related function: `tr::makeIfThenElse`.

### Learn someday

- [ ] [cannon.cc](../src/tiger/canon/canon.cc):83 Tree rewrite algorithm.
