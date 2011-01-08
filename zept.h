/*

vaguely python-ish language
main goal is <= 2k loc
1-pass compile to x64 or arm
no globals
vars are double or double[]. "stuff" is conv to list
len(L), push(L,x), pop(L), L[i]

    # recursive fib of arg passed in
    def fib(x):
        if x < 3:
            1
        else:
            fib(x - 1) + fib(x - 2)

    def __main__(args[]):
        print(fib(args[0]))
        
hrm

*/

int run(const char* code) {

}

