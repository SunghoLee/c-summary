# C Semantic Summary Extractor for JNI program analysis 

The C Semantic Summary Extractor is a tool that extracts semantic summaries related to the JNI interoperation from C/C++ source code, which is a part of JNI program analysis. Most things of this repo is cloned from the [Infer static analyzer repository](https://github.com/facebook/infer), since the tool is built on top of Infer.

Summaries extracted by the tool consists of JNI function call information including target JNI functions and abstract argument values at the call sites. The extracted summaries are translated to Java methods by JavaGenerator. By replacing C/C++ code in JNI programs with the generated Java methods, Java static analyzer can analyze the JNI programs. 

For more detail, please read the paper ["Broadening Horizons of Multilingual Static Analysis: Semantic Summary Extraction from C Code for JNI Program Analysis"](https://ieeexplore.ieee.org/abstract/document/9286029) that is presented in ASE'20.

## Installation

### Additional binary prerequisites
Install the following binaries using ```sudo apt install [package]```
- libgmp-dev 
- libsqlite3-dev 
- zlib1g-dev 
- libmpfr-dev
- ocamlbuild
- python 2.7

### Additional library prerequisites
Install the following libraries using ```opam install [library]```
- z3

### Installation steps
After installing the additional prerequisites, the remainig installation steps are completely same with the Infer static analyzer. See "Install Infer from source" in [INSTALL.md](https://github.com/facebook/infer/blob/master/INSTALL.md) to build Infer and this tool.

## License

C Semantic Summary Extractor is MIT-licensed.
