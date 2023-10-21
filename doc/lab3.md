## Overview of Bisonc++

The grammar file of bisonc++ consists of **only two sections**:

```
    Bisonc++ directives
    %%
    Grammar rules
```
There is no C declaration section and no section to define Additional C code, since bisonc++ thinks they're superfluous.

### Conflicts

- shift-reduce conflict: By default a shift is used, rather than a reduce.
- reduce-reduce conflict: the default action is to reduce by the rule listed first in the grammar.

### Directives

#### %token

```
  %token [ <type> ] terminal token(s)
```

+ Multiple tokens are separated by whitespace or commas.
+ \<type\>: optional. Must be a field in %union specification.

#### %union

```
%union union-definition body
```

Define the fields and their types. The field names can be used in %type and %token directives.

#### %type

Associating semantic values with (non)terminals.

```
%type <type> symbol-list
```

## Lab3 Specification

My grammar contains 5 shift-reduce conflicts and no reduce-reduce conflicts. All shift-reduce conflicts can be hacked by taking the shift action, except for this special case:

```yacc
lvalue: ID
  | lvalue [ exp ]
  ;

exp: ID [ exp ] OF exp;

```

Here is also a shift-reduce conflict, but considering parsing the sentence:

```
ID[exp]
```

It should be parsed as the `lvalue`, with a reduction from `ID` to `lvalue`. However, since bisonc++ prefers shift to reduction, the token `ID` won't be correctly reduced, therefore a syntax error would occur.

It's not easy to handle it for the LALR(1) parser, hence the parser cannot know whether `ID` should be reduced unless it looks ahead 4 tokens in this case. We can rewrite the grammar by adding a production:

```yacc
lvalue: ID [ exp ]
```

With the production, even the token `ID` isn't reduced to the non-terminal `lvalue`, the parser can still reduce `ID[exp]` to `lvalue` successfully.
