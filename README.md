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


## Code of Contribute
Work on you on branch and merge it into `main` before pushing. Don't work on `main` directly.
