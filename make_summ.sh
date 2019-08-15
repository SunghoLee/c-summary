#!/bin/bash

make
cd infer/src
dune build PostGlobal.exe
cd -
