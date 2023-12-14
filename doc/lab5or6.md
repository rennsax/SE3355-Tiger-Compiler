## TODO List

### Necessary

- [x] Follow static link. Record the static link access.
- [x] RBP and RSP: replace all occurrence of RBP (frame pointer)
- [x] Multiplication: `imulp s` implicitly use RAX and RDX, should I backup them?
  - Yes, they are backed up.
  - [ ] But is it necessary to backup RAX and RDX for `imulp` operation?
- [ ] For register allocation: caller-save and callee-save recall. (P244)
- [ ] Rewrite MunchInMemory to generate more complicated memory representations, based on [instruction.py](../scripts/lab5_test/instruction.py).
- [ ] The `dst` and `src` of some instructions should be considered again. Rewrite/Patch when necessary.

### Perplexed

- [ ] Will register allocation affect frame size? Should `procEntryExit3` handle it? If the frame size is expected to be increased, how to modify it?

### Optional

- [ ] Register allocation _for the tree_, integrated with Munch. (P257)
- [ ] Strength reduction: optimization for multiplication (maybe utilize the `lea` instruction).
