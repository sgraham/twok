#!/bin/bash
gcc -g -Wall zept_test.c -o zept_test && ./zept_test $*
#valgrind --quiet --leak-check=full ./zept_test $*
