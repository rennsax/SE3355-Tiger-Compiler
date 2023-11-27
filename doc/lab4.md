- [x] Named Type

- [x] Re-declaration

## Questions

### Named Type

The signature of `type::NameTy`:
```cpp
class NameTy : public Ty {
public:
  sym::Symbol *sym_;
  Ty *ty_;

  NameTy(sym::Symbol *sym, Ty *ty) : sym_(sym), ty_(ty) {}

  Ty *ActualTy() override;
};
```

The field `sym_` is so-called an **alias** of the type `ty_`. But is `ty_` the ultimate type? In other words, if we have a "chain" of `NameTy` like this,
  ```
  a -> b -> c -> int
  ```
what will the `NameTy` of `a` be like? `NameTy{"a", NameTy{"b", ...}}` or just `NameTy{"a", type::IntTy::Instance}`?

**Answer**: the definition of `ActualTy` interface has handled the problem of "chain type":

```cpp
Ty *Ty::ActualTy() { return this; }

Ty *NameTy::ActualTy() {
  assert(ty_ != this);
  return ty_->ActualTy();
}
```

Except for `NameTy`, the method `ActualTy` of each derived type of `Ty` just returns the inner type itself. When it comes to `NameTy`, it tracked the "actual type" of its member `ty_` recursively, until one of the descendants isn't `NameTy`.

### Re-declaration

Tiger has rules for local re-declarations (see P521). Briefly, Tiger allows declarations override previous declarations. The only exception is that in a sequence of mutually recursive functions/types, no duplicated names are allowed.

### Case 22

Some errors are not emitted.