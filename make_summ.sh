#!/bin/bash

make
cd infer/src
dune build GlobalCollector.exe
dune build PostGlobal.exe
dune build JavaGenerator.exe
cd -
