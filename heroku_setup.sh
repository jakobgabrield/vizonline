#!/bin/sh
sh install.sh < install.txt 
eval $(opam config env)
opam init --disable-sandboxing
opam install dune