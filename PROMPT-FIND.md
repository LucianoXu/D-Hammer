# Formalization Task
You should read the input text or pdf, extract the Dirac notation equations satisfying the requirements below, output it, and transform it into the domain-specific language code.

## Dirac notation requirements
You should investigate the context for the equation to check these requirements.
- It should be an equation for the common Dirac notation. It can contain summation \sum, but the value set shoule be simple.
- It should be an equation with no existence quantifier on variables. The equation can be a standalone formula, or a step in a long equational proof.
- It should not contain premises, such as:
  - The operator U is unitary, or Hermitian.
  - The equation for variables ... holds.
- It can have some symbol definitions. In this case, you should find the symbol definition and include it in the code encoding.

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

### Instructions for encoding
You should define or assum all the variables appear in the expression before the CheckEq command.


## Output format
The output should be a sequence of json object:
```
{
    "example" : [EXAMPLE],
    "encoding" : [ENCODING]
}
```
Here [EXAMPLE] is the conclusion of the extracted example in LaTeX format. [ENCODING] is the encoding for the example in the solver language. Each suitable example in the input should be output.

## Example
Here are some examples and their corresponding encodings.

{
    "example" : " Let $\ket{\Phi} = \sum_{i \in V}\ket{i}\ket{i}$ be the maximally entangled state on Hilbert space $\mathcal{H}_V\otimes\mathcal{H}_V$. Then for all operators $M \in \cL(\mathcal{H}_V)$,
    $$
    (M \otimes I) \ket{\Phi} = (I \otimes M^T) \ket{\Phi}.
    $$
    ",
    "encoding" : "
    Var T : INDEX.
    Var M : OTYPE[T, T].
    Def phi := idx T => Sum nv in USET[T], |(nv, nv)>.
    CheckEq (M * 1O[T]) (phi T) with (1O[T] * (TPO T T M)) (phi T).
    "
}
(examples end)

Give the encoding of the examples you find below: