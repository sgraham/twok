#!/bin/bash
clear
gcc -g -O0 -Wall zept_test.c zept_test2.c -o zept_test && \
valgrind --quiet --leak-check=full ./zept_test $*
sloccount zept.h | awk '/Total Physical/ {print "LOC: " $NF}'
