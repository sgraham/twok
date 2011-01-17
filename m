#!/bin/bash
clear
gcc -std=gnu89 -g -O0 -Wall -Wextra -Werror -ldl zept_test.c zept_test2.c -o zept_test && \
valgrind --quiet --leak-check=full ./zept_test $* && \
sloccount --duplicates zept.h | awk '/Total Physical/ {print "LOC: " $NF}'
