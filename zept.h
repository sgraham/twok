/*

vaguely python-ish language
main goal is <= 2k loc
1-pass compile to x64 or arm
no globals
vars are double or double[]. "stuff" is conv to list
len(L), push(L,x), pop(L), L[i]
unresolved uses are dlsym/GetProcAddress

    # recursive fibs up to arg passed
    def fib(x):
        if x < 3:
            1
        else:
            fib(x - 1) + fib(x - 2)

    def __main__(args[]):
        for x in range(args[0]):
            print(fib(x))
        
hrm

*/

// http://nothings.org/stb/stretchy_buffer.txt
// stretchy buffer // init: NULL // free: sbfree() // push_back: sbpush() // size: sbcount() //
#define sbfree(a)         ((a) ? free(stb__sbraw(a)),0 : 0)
#define sbpush(a,v)       (stb__sbmaybegrow(a,1), (a)[stb__sbn(a)++] = (v))
#define sbcount(a)        ((a) ? stb__sbn(a) : 0)
#define sbadd(a,n)        (stb__sbmaybegrow(a,n), stb__sbn(a)+=(n), &(a)[stb__sbn(a)-(n)])
#define sblast(a)         ((a)[stb__sbn(a)-1])

#include <stdlib.h>
#define stb__sbraw(a) ((int *) (a) - 2)
#define stb__sbm(a)   stb__sbraw(a)[0]
#define stb__sbn(a)   stb__sbraw(a)[1]

#define stb__sbneedgrow(a,n)  ((a)==0 || stb__sbn(a)+n >= stb__sbm(a))
#define stb__sbmaybegrow(a,n) (stb__sbneedgrow(a,(n)) ? stb__sbgrow(a,n) : 0)
#define stb__sbgrow(a,n)  stb__sbgrowf((void **) &(a), (n), sizeof(*(a)))

static void stb__sbgrowf(void **arr, int increment, int itemsize)
{
    int m = *arr ? 2*stb__sbm(*arr)+increment : increment+1;
    void *p = realloc(*arr ? stb__sbraw(*arr) : 0, itemsize * m + sizeof(int)*2);
    if (p) {
        if (!*arr) ((int *) p)[1] = 0;
        *arr = (void *) ((int *) p + 2);
        stb__sbm(*arr) = m;
    }
}

static char *cur, *ident;
static int ch, tok;
#define inp() ch = *cur++
#define isid() (isalnum(ch) || ch == '_')
enum { TOK_IDENT, TOK_NUM, TOK_IF, TOK_ELIF, TOK_ELSE, TOK_FOR, TOK_MAIN };
int next()
{
    while (isspace(ch))
    {
        inp();
        next();
    }
    ident = 0;
    tok = TOK_IDENT;
    if (isid())
    {
        printf("start tok\n");
        while (isid())
        {
            sbpush(ident, ch);
            inp();
        }
        sbpush(ident, 0);
        printf("  got: %s\n", ident);
    }
    return 0;
}

int funcs()
{
    next();
    return 0;
}

int zept_run(char* code)
{
    cur = code;
    inp();
    funcs();
    return 0;
}
