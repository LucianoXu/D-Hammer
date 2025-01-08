# Diracoq Project
GitHub: https://github.com/LucianoXu/diracoq


shell code to generate the ANTLR C++ files:

```
antlr4 -Dlanguage=Cpp src/ualg/ualg.g4 -o src/ualg/antlr_generated
```

To compile in VSCode:
- Install the Boost library (https://www.boost.org/).
- Generate the ANTLR C++ files using the command above.
- In VSCode, press`Ctrl + shift + P` or `Cmd + shift + P`, type and select `Tasks: Run Task`, and run the task `Compile Debug Version` or `Compile Release Version`.

## Mathematica
Version 14.0.0

## Dependency
- ANTLR4
- WSTP4

TODO:
...

## Model Prompts

### Result Formatting

ADD[SUM[USET[T], FUN[BASIS[T], SCR[-1, OUTER[KET[@0], BRA[@0]]]]], SUM[USET[T], FUN[BASIS[T], OUTER[KET[@0], BRA[@0]]]], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SCR[DOT[BRA[@1], MULK[A, KET[@0]]], OUTER[KET[@1], BRA[@0]]]]]]]]

Transform the above term into a readable expression. The meaning of the symbols are indicated by their names.

Explanations:

- ADD means addition, and ADD[a, b, c, ...] can be denoted as "a + b + c + ..."
- FUN means lambda abstraction
- KET means ket, and KET[i] can be denoted as |i>
- BRA means bra, and BRA[i] can be denoted as <i|
- OUTER means outer product.
- DOT means multiplication
- SCR means scalar multiplication, and SCR[a, b] can be denoted as "a . b" .
- @i are debruijn index numbers and can be left as it is.

Requirement:

Reformat the term as it is. Don't do any simplifications.