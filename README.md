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

## Run C Semantic Summary Extractor
Before extracting semantic summaries from C/C++ code, the capturing phase of Infer must be done. Please see [this material](https://fbinfer.com/docs/infer-workflow) to perform the capturing phase.

After then, we need five steps to extract semantic summaries and one more step to transform the summaries to Java methods. At first, the following five commands are used to extract semantic summaries from the captured C/C++ code:

```
# Pre-analysis to calculate alias relations
infer analyze -P --pp-only

# Pre-analysis to construct a global environment 
infer analyze -P --ssp-only
src/_build/opt/GlobalCollector.exe

# Main-analysis to extract semantic summaries
infer analyze -P --ss-only

# Post-analysis for global variables
src/_build/opt/PostGlobal.exe
```
The fourth command is to perform the main analysis that extracts semantic summaries, but there are two pre-analyses and one post-analysis. The first pre-analysis is to calculate alias relations, which helps the modular analysis (i.e. semantic summary extractor) to build a precise initial heap state for each C/C++ function. The second pre-analysis and the the post-analysis handle global variables in a special way, since the modular analysis is hard to analyze global variables correctly; it infers types of all global variables, and joins all the possible abstract values stored to the global variables. 

Finally, to transform the extracted semantic summaries to Java methods, try the following command:
```
src/_build/default/JavaGenerator.exe
```
The ```JavaGenerator``` takes semantic summaries stored in the Infer's database, transforms them to Java methods, and generates Java files. 

The ```GlobalCollector.exe```, ```PostGlobal.exe```, and ```JavaGenerator.exe``` would be generated in the ```src/_build/default``` directory after compilation. And a script file ```run.sh``` contains all the steps for the semantic summary extraction, but some lines must be modified to make the script to run on your environment.

## License

C Semantic Summary Extractor is MIT-licensed.
