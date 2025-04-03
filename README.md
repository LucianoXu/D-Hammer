# D-Hammer Project
A solver for Dirac notation equations written in C++.

GitHub: https://github.com/LucianoXu/D-Hammer


## Installation and Requirements

### Prerequisite
- Compilation of the C++ backend requires a compiler that supports the C++23 standard.
- Java installation to generate the ANLTR4 parser file.
- CMake installation, version 3.28 or later.

### Installation
1. Install the C++ Boost library in a way that can be discovered by `find_package` in CMake. For example, on MacOS:
```
brew install boost
```

3. Clone the repository using 
```
git clone --recurse-submodules https://github.com/LucianoXu/equationNN.git
```
navigate to the project root folder, and check out the `main` branch.

1. Install the Python library dependencies using
```
pip install -r requirements.txt
```
Install the `elab` library in the editable way using
```
pip install -e elab
```

1. Compile the C++ backend. You can press `Cmd+Shift+B` and run the *Compile All Versions* task. It automatically creates the *build* directory and compiles the Python library. The compilation requires to link to the Python installation where the later experiments carries out. The CMakeLists will find package of Python directly. To specify the Python installation, use the `cmake` flag `-DPYTHON_EXECUTABLE=...` to specify the `python3` executable.

2. Run the tests to validate installation. For C++ tests, execute
```
ctest --test-dir build/Debug
ctest --test-dir build/Release
```
For python tests, execute
```
pytest
```
And if all tests are passed successfully, it should mean that the installation succeeds.
The tests can also be initiated by the *testing* sidebar in VSCode.

## Experiments

The `main` python script at the project root folder act as the main entry of different kinds of experiments. 
The script accepts extra command line arguments to specify the experiment (task) to execute and corresponding arguments.
See the task scripts to know the command line arguments. In the `main` entry, there is also an example command under each task.

## VSCode configurations
The .vscode directory contains the configurations for compilation/debugging within VSCode.


## Code of Contribute
Work on you on branch and merge it into `main` before pushing. Don't work on `main` directly.
Follow the structure in `main` and `pysrc/tasks` to archive the valuable experiments you composed.









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