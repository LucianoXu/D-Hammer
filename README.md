# D-Hammer Project
A solver for Dirac notation equations written in C++.

GitHub: https://github.com/LucianoXu/D-Hammer


## Installation and Requirements

### Platform
MacOS-arm or Linux-x86

### Prerequisite
- The `clang++` compiler with C++23 standard. If you want to try with other compiler you can change the "Set the compiler" line in the `CMakeLists.txt` file, but with `g++` it does not work and with other compilers it is not tested.
- The C++ Boost library (https://www.boost.org/) installed in a way that can be discovered by `find_package` in CMake.
- Java installation to generate the ANLTR4 parser file.
- CMake installation, version 3.28 or later.
- Install WolframEngine (https://www.wolfram.com/engine/), run `wolframscript` and follow the instruction to activate it using a Wolfram ID.

In Arch Linux you can install all the dependencies and activate WolframEngine using the following commands.
```bash
sudo pacman -S cmake boost-libs boost jdk-openjdk clang
paru -S wolframengine
wolframscript
```

### Compilation and usage

1. Clone the repository using `git clone https://github.com/LucianoXu/D-Hammer`, navigate to the project root folder, and check out the `main` branch.

2. Compile the C++ backend. In VSCode you can press `Cmd+Shift+B` and run the *Compile Release Versions* task. It automatically creates the *build* directory and compiles the release version. From the command line you can run the following commands.
```bash
cmake -S . -B build/Release -DCMAKE_BUILD_TYPE=Release
cmake --build build/Release
```

3. Run the tests to validate the compilation. For C++ tests, execute `ctest --test-dir build/Release`,
and if all tests are passed successfully, it should mean that the compilation is okay.
The tests can also be initiated by the *testing* sidebar in VSCode.

4. For running the tool run the following command.
```bash
build/Release/cpp/TOP -linkmode launch -linkname wolfram -wstp
```
This will give you and interactive shell where you can run examples.


## VSCode configurations
The .vscode directory contains the configurations for compilation/debugging within VSCode.


## Solver language
The solver langauge is defined as follows.

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


## Code of Contribute
Work on you on branch and merge it into `main` before pushing. Don't work on `main` directly.
