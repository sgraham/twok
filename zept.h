/* pull stuff itno structure and memset it
 * get branching to main working and return hardcoded val
 * */
/*

vaguely python-ish language
main goal is <= 1k loc
1-pass compile to x64 or arm
vars are word (bottom bit set) or *word (bottom bit clear).
"str" is conv to list
len(L), push(L,x), pop(L), L[i]
unresolved uses are dlsym/GetProcAddress
no globals
indent always 4 spaces
gc walks global ptr table

    # recursive fibs up to arg passed
    def fib(x):
        if x < 3:
            1
        else:
            fib(x - 1) + fib(x - 2)

    def __main__(*args):
        for x in range(args[0]):
            print(fib(x))
        
making lists dynamically:
    x = [1,2,3]
===
    x = NULL
    push(x, 1)
    push(x, 2)
    push(x, 3)

so [] === NULL, data lives in regular malloc'd space
    

*/

// http://nothings.org/stb/stretchy_buffer.txt
// stretchy buffer // init: NULL // free: sbfree() // push_back: sbpush() // size: sbcount() //
#define sbfree(a)         ((a) ? free(stb__sbraw(a)),0 : 0)
#define sbpush(a,v)       (stb__sbmaybegrow(a,1), (a)[stb__sbn(a)++] = (v))
#define sbcount(a)        ((a) ? stb__sbn(a) : 0)
#define sbadd(a,n)        (stb__sbmaybegrow(a,n), stb__sbn(a)+=(n), &(a)[stb__sbn(a)-(n)])
#define sblast(a)         ((a)[stb__sbn(a)-1])

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include <string.h>
#include <sys/mman.h>
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

static char *file, *cur, *ident, *codeseg, *codep, *entry;
static int ch, tok, tokn, indentLevel;
#define inp() ch = *cur++
#define isid() (isalnum(ch) || ch == '_')
enum { TOK_UNK, TOK_EOF, TOK_IDENT, TOK_NUM, TOK_IF, TOK_ELIF, TOK_ELSE, TOK_FOR, TOK_MAIN };
void indedent(int delta);
static jmp_buf errBuf;
static char errorText[512];
#define ALLOC_SIZE 1<<17

#define ob(b) (*codep++ = b)
#define out32(n) do { int _ = (n); ob(_&0xff); ob((_&0xff00)>>8); ob((_&0xff0000)>>16); ob((_&0xff000000)>>24); } while(0);
void loadconst32(int n)
{
    ob(0xb8); /* mov eax, N */
    out32(n);
}
void prolog()
{
    ob(0x55); /* push rbp */
    ob(0x48); ob(0x89); ob(0xe5); /* mov rbp, rsp */
}
void next(int skipWS)
{
    while (skipWS && ch == ' ')
    {
        inp();
        next(skipWS);
    }
    sbfree(ident);
    ident = 0;
    tok = ch;
    if (isid())
    {
        while (isid())
        {
            sbpush(ident, ch);
            inp();
        }
        if (isdigit(tok))
        {
            tokn = strtol(ident, 0, 0);
            tok = TOK_NUM;
        }
        else
        {
            /* todo; symtab entry */
            tok = TOK_IDENT;
        }
    }
    else
    {
        inp();
        if (tok == 0)
            tok = TOK_EOF;
    }
    sbpush(ident, 0);
}

void getRowColTextFor(int offset, int* line, int* col, char** linetext)
{
    char* cur = file;
    *line = 1;
    *col = 1;
    *linetext = cur;
    while (offset-- >= 0)
    {
        if (*cur++ == '\n')
        {
            *linetext = cur;
            *line += 1;
            *col += 1;
        }
    }
}

void error(char *fmt, ...)
{
    va_list ap;
    int line, col;
    char* text, *eotext;
    char tmp[256];

    va_start(ap, fmt);
    getRowColTextFor(cur - file, &line, &col, &text);
    sprintf(errorText, "line %d:\n", line);
    eotext = text;
    while (*eotext != '\n' && *eotext != 0) ++eotext;
    strncat(errorText, text, eotext - text + 1);
    while (col--) strcat(errorText, " "); /* todo; ! */
    strcat(errorText, "^\n");
    vsprintf(tmp, fmt, ap);
    strcat(errorText, tmp);
    strcat(errorText, "\n");
    longjmp(errBuf, 1);
    va_end(ap);
}

void skip(int c, int skipWS)
{
    if (tok != c)
        error("'%c' expected, got '%s'\n", c, ident);
    next(skipWS);
}

void indedent(int delta)
{
    int i;
    for (i = 0; i < (indentLevel + delta) * 4; ++i) skip(' ', 0);
    indentLevel += delta;
}
void readIndent()
{
    char* start = cur;
    int count = 0;
    for (; tok == ' '; ++count) next(0);
    if (count % 4 != 0 || count / 4 > indentLevel)
    {
        cur = start;
        error("bad indent");
    }
    indentLevel = count / 4;
}

void expr()
{
    ;//printf("expression: ");
    if (tok == TOK_NUM)
        loadconst32(tokn);
    else if (tok == TOK_IDENT)
        ; //printf("ident: %s\n", ident);
    next(0); /* skip the thing */
    skip('\n', 0);
    readIndent();
}

void block()
{
    int startIndent;
    skip('\n', 0);
    indedent(1);
    startIndent = indentLevel;
    while (indentLevel >= startIndent)
        expr();
}

int toplevel()
{
    int offset;
    while (tok != TOK_EOF)
    {
        next(0);
        //printf("FUNC: '%s'\n", ident);
        if (strcmp(ident, "__main__") == 0) entry = codep;
        next(1); skip('(', 1);
        offset = 0;
        while (tok != ')')
        {
            offset++;
            next(1);
            if (tok == ')')
                next(1);
        }
        skip(')', 1); skip(':', 1);
        block();
    }
}

int zept_run(char* code)
{
    int ret;
    if (setjmp(errBuf) == 0)
    {
        cur = file = code;
        indentLevel = tok = 0;
        ident = 0;
        errorText[0] = 0;
        codeseg = codep = mmap(0, ALLOC_SIZE, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
        prolog();
        inp();
        toplevel();
        *codep++ = 0xc9;
        *codep++ = 0xc3;
        ret = ((int (*)())entry)();
    }
    else
    {
        ret = -1;
    }
    sbfree(ident);
    munmap(codeseg, ALLOC_SIZE);
    return ret;
}

