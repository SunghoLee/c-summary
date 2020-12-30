# C Semantic Summary Extractor for JNI program analysis 

The C Semantic Summary Extractor is a tool that extracts behavioral summaries from C/C++ source code, which is a part of JNI program analysis. Most of things in this repo is cloned from the Infer static analyzer repository, since the tool is built on top of Infer.
Summaries extracted by the tool consists of JNI function call information that is a pair of JNI functions it calls and abstract argument values at the call sites. For more detail, please read the paper presented in ASE'20.

## Installation

See [INSTALL.md](https://github.com/facebook/infer/blob/master/INSTALL.md) to build Infer and this tool.

## License

Infer and C Semantic Summary Extractor is MIT-licensed.

Note: Enabling Java support may require you to download and install 
components licensed under the GPL.
