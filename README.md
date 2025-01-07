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