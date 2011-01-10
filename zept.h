/* zept-0.10 - public domain, pythonish script lang - http://h4ck3r.net/zept.h
   Scott Graham 2011 <scott.zept@h4ck3r.net>
                                    No warranty implied; use at your own risk.

Before including,

    #define ZEPT_DEFINE_IMPLEMENTATION

in the file that you want to have the implementation.


ABOUT:

    Native compile on x64, ARM (not yet)
    < 1k LOC (`sloccount zept.h`)
    No external dependencies

    ("zepto" is the SI prefix for 10e-21)


TODO:

    everything

*/

#ifndef INCLUDED_ZEPT_H
#define INCLUDED_ZEPT_H
#ifdef __cplusplus
extern "C" {
#endif

extern int zeptRun(char* code);

#ifdef __cplusplus
}
#endif
#endif /* INCLUDED_ZEPT_H */

#ifdef ZEPT_DEFINE_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include <string.h>
#include <ctype.h>

typedef struct Token {
    int type, pos;
    union {
        char str[32];
        int tokn;
    } data;
} Token;

typedef struct Context {
    Token* tokens;
    int curtok;
    char *input, *codeseg, *codep, *entry;
    jmp_buf errBuf;
    char errorText[512];
} Context;

static Context C;
static void suite();


/*
 * misc utilities.
 */

/* simple vector based on http://nothings.org/stb/stretchy_buffer.txt */
#define zvfree(a)                   ((a) ? free(zv__zvraw(a)),(void*)0 : (void*)0)
#define zvpush(a,v)                 (zv__zvmaybegrow(a,1), (a)[zv__zvn(a)++] = (v))
#define zvpop(a)                    (zv__zvn(a)-=1)
#define zvsize(a)                   ((a) ? zv__zvn(a) : 0)
/*#define zvadd(a,n)                  (zv__zvmaybegrow(a,n), zv__zvn(a)+=(n), &(a)[zv__zvn(a)-(n)])*/
#define zvlast(a)                   ((a)[zv__zvn(a)-1])
#define zvfindnp(a,i,n,psize)       (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),n,psize))
#define zvcontainsnp(a,i,n,psize)   (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),n,psize)!=-1)
#define zvcontainsp(a,i,psize)      (zvcontainsnp((a),i,zv__zvn(a),psize))
#define zvcontains(a,i)             (zvcontainsp((a),i,sizeof(*(a))))

#define zv__zvraw(a) ((int *) (a) - 2)
#define zv__zvm(a)   zv__zvraw(a)[0]
#define zv__zvn(a)   zv__zvraw(a)[1]

#define zv__zvneedgrow(a,n)  ((a)==0 || zv__zvn(a)+n >= zv__zvm(a))
#define zv__zvmaybegrow(a,n) (zv__zvneedgrow(a,(n)) ? zv__zvgrow(a,n) : 0)
#define zv__zvgrow(a,n)  zv__zvgrowf((void **) &(a), (n), sizeof(*(a)))

static void zv__zvgrowf(void **arr, int increment, int itemsize)
{
    int m = *arr ? 2*zv__zvm(*arr)+increment : increment+1;
    void *p = realloc(*arr ? zv__zvraw(*arr) : 0, itemsize * m + sizeof(int)*2);
    if (p) {
        if (!*arr) ((int *) p)[1] = 0;
        *arr = (void *) ((int *) p + 2);
        zv__zvm(*arr) = m;
    }
}
static int zv__zvfind(char* arr, char* find, int itemsize, int n, int partialsize)
{
    int i;
    for (i = 0; i < n; ++i)
        if (memcmp(&arr[i*itemsize], find, partialsize) == 0)
            return i;
    return -1;
}

#define CURTOK (C.tokens[C.curtok])
#define CURTOKt (CURTOK.type)

static void geterrpos(int offset, int* line, int* col, char** linetext)
{
    char* cur = C.input;
    *line = 1; *col = 1;
    *linetext = cur;
    while (--offset >= 0)
    {
        *col += 1;
        if (*cur++ == '\n')
        {
            *linetext = cur;
            *line += 1;
            *col = 1;
        }
    }
}

/* report error message and longjmp */
static void error(char *fmt, ...)
{
    va_list ap;
    int line, col;
    char* text, *eotext;
    char tmp[256];

    va_start(ap, fmt);
    if (C.curtok < zvsize(C.tokens)) /* for errors after parse finished */
    {
        geterrpos(CURTOK.pos, &line, &col, &text);
        sprintf(C.errorText, "line %d, col %d:\n", line, col);
        eotext = text;
        while (*eotext != '\n' && *eotext != 0) ++eotext;
        strncat(C.errorText, text, eotext - text + 1);
        while (--col) strcat(C.errorText, " "); /* todo; ! */
        strcat(C.errorText, "^\n");
    }
    vsprintf(tmp, fmt, ap);
    strcat(C.errorText, tmp);
    strcat(C.errorText, "\n");
    longjmp(C.errBuf, 1);
    va_end(ap);
}


/*
 * tokenize. build a zv of Token's for rest. indent/dedent is a bit icky.
 */

#define KWS " if elif else or for def return mod and not print << >> <= >= == != "
#define KW(k) (strstr(KWS, #k " ") - KWS)
enum { T_UNK, T_IDENT = 1<<8, T_END, T_NL, T_NUM, T_INDENT, T_DEDENT };
static Token* tokSetStr(Token* t, char* str)
{
    if (strlen(str) >= sizeof(t->data.str)) error("identifer too long");
    strcpy(t->data.str, str);
    t->data.str[sizeof(t->data.str) - 1] = 0;
    return t;
}
#define TOK(t) do { Token _ = { t, startpos - C.input }; zvpush(C.tokens, *tokSetStr(&_, #t)); } while(0)
#define TOKI(t, s) do { Token _ = { t, startpos - C.input }; zvpush(C.tokens, *tokSetStr(&_, s)); } while(0)
#define TOKN(t, v) do { Token _ = { t, startpos - C.input }; _.data.tokn=v; zvpush(C.tokens, _); } while(0)
#define isid(ch) (isalnum(ch) || ch == '_')

static void tokenize()
{
    char *pos = C.input, *startpos;
    int i, tok, column;
    int *indents = 0;
    char *ident = 0;

    zvpush(indents, 0);

    for (;;)
    {
        column = 0;
        while (*pos == ' ') { pos++; column++; }
        startpos = pos;

        if (*pos == 0)
        {
donestream: for (i = 1; i < zvsize(indents); ++i)
                TOK(T_DEDENT);
            TOK(T_END);
            zvfree(indents);
            zvfree(ident);
            return;
        }

        if (*pos == '#' || *pos == '\r' || *pos == '\n')
        {
            if (*pos == '#')
                while (*pos++ != '\n') {}
            else
                ++pos;
        }
        if (column > zvlast(indents))
        {
            zvpush(indents, column);
            TOK(T_NL);
            TOK(T_INDENT);
        }
        while (column < zvlast(indents))
        {
            if (!zvcontains(indents, column))
                error("unindent does not match any outer indentation level");
            zvpop(indents);
            TOK(T_DEDENT);
        }

        while (*pos != '\n')
        {
            while (*pos == ' ') ++pos;
            startpos = pos;
            ident = zvfree(ident);
            tok = *pos;
            if (isid(*pos))
            {
                while (isid(*pos))
                    zvpush(ident, *pos++);
                zvpush(ident, 0);
                if (isdigit(tok))
                    TOKN(T_NUM, strtol(ident, 0, 0));
                else
                {
                    tok = T_IDENT;
                    if (strstr(KWS, ident)) tok = strstr(KWS, ident) - KWS;
                    TOKI(tok, ident);
                }
            }
            else if (*pos == '#')
            {
                while (*pos != '\n' && *pos != 0) ++pos;
                break;
            }
            else
            {
                if (!*pos) goto donestream;
                char tmp[3] = { *pos++, 0, 0 };
                if (0) {}
                #define twochar(t) else if (*(pos-1) == #t[0] && *pos == #t[1]) { tmp[1] = *pos++; TOKI(KW(t), tmp); }
                    twochar(<<) twochar(>>)
                    twochar(<=) twochar(>=)
                    twochar(!=) twochar(==)
                #undef twochar
                else TOKI(tmp[0], tmp);
            }
        }
        ++pos;
    }
}


/*
 * code generation 
 */
#define ob(b) (*C.codep++ = b)
#define out32(n) do { int _ = (n); ob(_&0xff); ob((_&0xff00)>>8); ob((_&0xff0000)>>16); ob((_&0xff000000)>>24); } while(0);

static void g_loadconst32(int n)
{
    ob(0xb8); /* mov eax, N */
    out32(n);
}

static void g_prolog()
{
    ob(0x55); /* push rbp */
    ob(0x48); ob(0x89); ob(0xe5); /* mov rbp, rsp */
}

static void g_leave_ret() { ob(0xc9); ob(0xc3); } /* leave; ret */

#define put32(p, n) (*(int*)(p) = (n))

static char* g_test(int NZ)
{
    ob(0x85); ob(0xc0); /* test eax, eax */
    ob(0x0f); ob(0x84 + NZ); /* jz/jnz rrr */
    out32(0);
    return C.codep - 4;
}

static void g_fixup1(char* p, char* to)
{
    put32(p, to - p - 4);
}

static void g_fixup(char* p) { g_fixup1(p, C.codep); }

static void g_save() { ob(0x50); } /* push eax */

static void g_restorealt() { ob(0x59); } /* pop ecx */

static void g_cmp(int CC)
{
    ob(0x39); ob(0xc1); /* cmp ecx, eax */
    g_loadconst32(0);
    ob(0x0f); ob(0x90 + CC); /* setxx al */
    ob(0xc0);
}


/*
 * parsing
 */
#define NEXT() do { if (C.curtok >= zvsize(C.tokens)) error("unexpected end of input"); C.curtok++; } while(0)
#define SKIP(t) do { if (CURTOKt != t) error("'%c' expected, got '%s'", t, CURTOK.data.str); NEXT(); } while(0)

static void atom()
{
    if (CURTOKt == T_NUM)
    {
        g_loadconst32(CURTOK.data.tokn);
        NEXT();
    }
    else error("unexpected atom");
}

static void comparison()
{
    struct { char op; char cc; } cmptoks[] = { /* rhs is very platform-specific, the setcc op for each */
        { '<',    0xc },
        { '>',    0xf },
        { KW(<=), 0xe },
        { KW(>=), 0xd },
        { KW(==), 4 },
        { KW(!=), 5 },
    };
    int cmptoki;
    atom();
    while ((cmptoki = zvfindnp(cmptoks, CURTOKt, 6, 1)) != -1)
    {
        NEXT();
        g_save();
        atom();
        g_restorealt();
        g_cmp(cmptoks[cmptoki].cc);
    }
}

static void stmt()
{
    char *off0, *off1;
    if (CURTOKt == KW(return))
    {
        SKIP(KW(return));
        g_loadconst32(CURTOK.data.tokn);
        g_leave_ret();
        NEXT();
    }
    else if (CURTOKt == KW(print))
    {
        SKIP(KW(print));
        NEXT();
    }
    else if (CURTOKt == KW(if))
    {
        SKIP(KW(if));
        comparison();
        off0 = g_test(0);
        suite();
        g_fixup(off0);
        off1 = 0;
        while (CURTOKt == KW(elif))
        {
            comparison();
            suite();
        }
        /*
        if (CURTOKt == KW(else))
            suite();
        */
    }
    else if (CURTOKt == T_NL) error("bad indent");
    else error("expected stmt");
}

static void suite()
{
    SKIP(':');
    SKIP(T_NL);
    SKIP(T_INDENT);
    stmt();
    while (CURTOKt != T_DEDENT)
        stmt();
    SKIP(T_DEDENT);
}

static void funcdef()
{
    SKIP(KW(def));
    SKIP(T_IDENT);
    if (strcmp(C.tokens[C.curtok - 1].data.str, "__main__") == 0) C.entry = C.codep;
    SKIP('(');
    SKIP(')');
    g_prolog();
    suite();
    g_leave_ret();
}

static void fileinput()
{
    while (CURTOKt != T_END)
    {
        if (CURTOKt == T_NL) NEXT();
        else funcdef();
    }
    SKIP(T_END);
    if (C.curtok != zvsize(C.tokens)) error("unexpected extra input");
}

#if __unix__ || (__APPLE__ && __MACH__)
    #include <sys/mman.h>
    static void* zept_allocExec(int size) { return mmap(0, size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0); }
    static void zept_freeExec(void* p, int size) { munmap(p, size); }
#elif _WIN32
    #include <windows.h>
    static void* zept_allocExec(int size) { error("todo;"); }
    static void zept_freeExec(void* p, int size) { error("todo;"); }
#endif


/*
 * main api entry point
 */
int zeptRun(char* code)
{
    int ret;
    memset(&C, 0, sizeof(C));
    C.input = code;
    int allocSize = 1<<17;
    if (setjmp(C.errBuf) == 0)
    {
        tokenize();
#if 1 /* dump tokens generated from stream */
        { int j;
        for (j = 0; j < zvsize(C.tokens); ++j)
        {
            if (C.tokens[j].type == T_NUM)
                printf("%d: %d %d\n", j, C.tokens[j].type, C.tokens[j].data.tokn);
            else
                printf("%d: %d %s\n", j, C.tokens[j].type, C.tokens[j].data.str);
        }}
#endif
        C.codeseg = C.codep = zept_allocExec(allocSize);
        fileinput();
#if 0 /* dump disassembly of generated code, needs ndisasm in path */
        { FILE* f = fopen("dump.dat", "wb");
        fwrite(C.codeseg, 1, C.codep - C.codeseg, f);
        fclose(f);
        ret = /* warning suppress */ system("ndisasm -b64 dump.dat"); }
#endif
        if (!C.entry) error("no entry point '__main__'");
        ret = ((int (*)())C.entry)();
    }
    else
    {
        ret = -1;
    }
    zvfree(C.tokens);
    zept_freeExec(C.codeseg, allocSize);
    return ret;
}

#endif /* ZEPT_DEFINE_IMPLEMENTATION */
