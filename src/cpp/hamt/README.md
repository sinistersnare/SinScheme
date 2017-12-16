

A simple templated HAMT (Bagwell 2001) implementation. A functional, immutable hashmap/hashset for functional programming in C++. Relies on the Boehm GC for C/C++. Copyright 2017 Thomas Gilray, Kristopher Micinski---see LICENSE.md for license and terms of use.


To build and run tests to get started, install Boehm GC from https://github.com/ivmai/bdwgc/ and follow the instructions to build it with pthread support. The provided Makefile assumes the static library is installed at /usr/local/lib/libgc.a and that the include folder is at relative path ../bdwgc/include/


Then you may simply run:

$ make clean; make

$ ./test_hamt





