# Generation Task
You should generate the correct Dirac notation equations satisfying requirements below, in the defined solver language.

## Dirac notation requirements
- It should be an equation for the common Dirac notation. It can contain summation \sum, but the value set shoule be simple.
- It should be an equation with no existence quantifier on variables. The equation can be a standalone formula, or a step in a long equational proof.
- It should not contain premises, such as:
  - The operator U is unitary, or Hermitian.
  - The equation for variables ... holds.
- It can have some symbol definitions. In this case, you should include it in the encoding.

## Solver language
You should encode the equation in the solver langauge defined as follows.

The code in the solver language is a sequence of commands, including:
- `Var ID : term.` Assume the variable with name `ID` to be type `term`.
- `Def ID := term.` Define the variable `ID` to be the term `term`.
- `CheckEq term1 with term2.` Check whether `term1` and `term2` are equivalent.

Here `ID` is the identifier, and `term` is the grammar for terms.
In the internal language, the term grammar is defined as
```
term := ID | ID '[' term ( ',' term)* ']'
```
i.e., the LISP-like expression.

The solver language is separated into 3 layers: The *index*, the *typed*, and the *term*. 

### Index
Indices are defined as
```
index := x | index * index
```
Here x is a variable, and index * index represents the product type. index is used in two ways:
- denote the type (basis set) for the Hilbert space, or
- denote the type of index value sets in summation.

### Type
Types are defined as
```
type := BASIS[index]            # basis in index
        | STYPE                 # scalars
        | KTYPE[index]          # ket, Hilbert space by index
        | BTYPE[index]          # bra, Hilbert space by index
        | OTYPE[index1, index2] # linear operator, index2 as domain and index1 as codomain
        | type1 -> type2        # function types
        | forall x.type         # function types for index arguments
        | SET[index]            # set of index type
```

### Term
The terms for Dirac notation are defined as
```
term := x # variable
        | fun x : T => term     # function
        | idx x => term         # function for index
        | term1 term2           # application. Depending on the type, it can denote function application, bra-ket inner product, operator-ket multiplication, scaling and so on.
        | (e1, e2)              # the pair basis for product type
        | 0 | 1                 # const symbols
        | term1 + ... + termn   # addition
        | term1 * ... * termn   # multiplication
        | term^*                # (scalar) conjugate
        | 1O[index]             # identity operator on index
        | |term>                # ket on basis term
        | <term|                # bra on basis term
        | term^D                # conjugate transpose
        | term1 . term2         # scaling term2 with scalar term1
        | term1 * term2         # tensor product (for suitable type)
        | USET[index]           # the universal set of index
        | term1 * term2         # the Cartesian product of two sets
        | Sum i in term1, term2 # summation over index i in set term1, on term2
```

## Output format
The output should be a sequence encodings.
The provided examples should be as unique as possible.


## Example
Here are some examples.

Var T : INDEX.
Var M : OTYPE[T, T].
Def phi := idx T => Sum nv in USET[T], |(nv, nv)>.
CheckEq (M * 1O[T]) (phi T) with (1O[T] * (TPO T T M)) (phi T).

Var T1 : INDEX.
Var T2 : INDEX.
Var m : INDEX.
Var M : SET[m].
Var a : BASIS[m] -> STYPE.
Var A : BASIS[m] -> OTYPE[T1, T2].
CheckEq (Sum i in M, (a i).(A i))^D with Sum i in M, (a i)^* . (A i)^D.

Var m : INDEX.
Var M : SET[m].
Def P := Sum i in M, |i> <i|.
CheckEq P P with P.

Var T1 : INDEX.
Var T2 : INDEX.
Var s : BASIS[T1].
Var t : BASIS[T2].
Var Q : REG[T1].
Var R : REG[T2].
CheckEq |s>_Q * |t>_R with |(s, t)>_(Q, R).

