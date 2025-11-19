Harness Core
============

Harness Core is a term rewrite system for the following language:

```
Term ::=
| Var ::= Name
| Ref ::= "@" Name
| Lam ::= "λ" Name "." Term
| App ::= "(" Term " " Term ")"
| Ctr ::= "#" Name "{" [Term] "}"
| Mat ::= "λ" "{" "#" Name ":" Term ";"? Term "}"

Where:
- Letter ::= x ∈ "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
- Name   ::= [Letter]
- [X]    ::= "" | X ","? [X]

Such that:
- Variables are affine: they must occur at most once.
- Scopes range globally: variables can occur anywhere.
```

Interactions
------------

```
(λx.f a)
-------- app-lam
x ← a
f

(λ{#A:f;<>:g} #B{x,y,z,...})
---------------------------- app-mat
if #A == #B:
  (f x y z ...)
else:
  (g x y z ...)
```
