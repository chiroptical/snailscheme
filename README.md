# Snailscheme

Snailscheme is a minimal Scheme interpreter to highlight the simplicity of
writing an interpreter in Haskell. The implementation and, forthcoming,
tutorial are meant to guide someone who has some Haskell experience but is
interested in learning about interpreters. We use very simple Haskell and best
in class libraries to provide the groundwork for building interpreters quickly.
You shouldn't view Snailscheme as "Write You a Scheme, Version 3". It is less
interesting for us to build a compliant Scheme than presenting what it takes to
build an interpreter. That being said, we will make it obvious when we deviate
from Scheme.

## Plans

- [ ] Finish the interpreter
- [ ] Evalutate the reference implementation
- [ ] Add context and examples to each and every function
- [ ] Finalize a correct test suite for the interpreter

## Goals

- Simple: no fancy Haskell is necessary to build an interpreter
- Well commented: one should easily be able to understand every function's purpose
- Well tested: property and golden tests ensure correctness

## Motivations

This project was motivated from [Write Yourself a Scheme in 48 Hours][wyas],
[Write You a Scheme, Version 2][wyas2], and [Intrigue][intrigue].

[wyas]: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
[wyas2]: https://www.wespiser.com/writings/wyas/home.html
[intrigue]: https://github.com/Kleidukos/Intrigue
