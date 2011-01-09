#!/bin/bash
clear
gcc -g -O0 -Wall zept_test.c -o zept_test && \
valgrind --quiet --leak-check=full ./zept_test $*
sloccount zept.h | awk '/Total Physical/ {print "LOC: " $NF}'
