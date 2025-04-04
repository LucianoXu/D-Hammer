# D-Hammer Project
A solver for Dirac notation equations written in C++.

GitHub: https://github.com/LucianoXu/D-Hammer


## Installation and Requirements

### Platform
MacOS-arm or Linux-x86

### Prerequisite
- Compilation of the C++ backend requires a compiler that supports the C++23 standard.
- The C++ Boost library (https://www.boost.org/) installed in a way that can be discovered by `find_package` in CMake.
- Java installation to generate the ANLTR4 parser file.
- CMake installation, version 3.28 or later.

### Installation

1. Clone the repository using 
`git clone https://github.com/LucianoXu/D-Hammer`, navigate to the project root folder, and check out the `main` branch.

2. Compile the C++ backend. You can press `Cmd+Shift+B` and run the *Compile Release Versions* task. It automatically creates the *build* directory and compiles the release version.

3. Install the WolframEngine 14.1 from https://www.wolfram.com/engine/ and follow the instruction to activate it using a Wolfram ID.

4. Run the tests to validate installation. For C++ tests, execute
`
ctest --test-dir build/Release
`,
and if all tests are passed successfully, it should mean that the installation succeeds.
The tests can also be initiated by the *testing* sidebar in VSCode.

## VSCode configurations
The .vscode directory contains the configurations for compilation/debugging within VSCode.


## Code of Contribute
Work on you on branch and merge it into `main` before pushing. Don't work on `main` directly.