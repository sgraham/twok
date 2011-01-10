/* zept-0.10 - public domain, python-ish script lang - http://h4ck3r.net/#Zept
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


NOTES: (mostly internal)

    register usage:


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
#include <stdint.h>
#include <assert.h>
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

typedef struct Value {
    int type;
    union {
        int i;
        char* addr;
    } data;
} Value;
enum { V_CONST = 0x1, V_LVAL = 0x2, V_CMP = 0x4 };
enum { R_0 = 0, R_1 = 1, R_2 = 2, R_ANY };
#define VAL(t, d) do { Value _ = { (t), { (d) } }; if (zvsize(C.vst) && zvlast(C.vst).type == V_CMP) g_rval(R_ANY); zvpush(C.vst, _); } while(0)

typedef struct Context {
    Token* tokens;
    Token** locals;   /* array of pointers to above (which won't be realloced at this point), index is offset from rbp */
    int curtok, scanningForLocals;
    char *input, *codeseg, *codep, *entry;
    Value* vst;
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
#define zvpop(a)                    (assert(zv__zvn(a) > 0), zv__zvn(a)-=1)
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

#define PREVTOK (C.tokens[C.curtok - 1])
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

#define KWS " if elif else or for def return mod and not print pass << >> <= >= == != "
#define KW(k) ((strstr(KWS, #k " ") - KWS) + T_KW)
enum { T_UNK, T_KW=1<<7, T_IDENT = 1<<8, T_END, T_NL, T_NUM, T_INDENT, T_DEDENT };
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
            if (!zvcontains(indents, column)) error("unindent does not match any outer indentation level");
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
                    if (strstr(KWS, ident)) tok = strstr(KWS, ident) - KWS + T_KW;
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
enum { REG_SIZE = 4 }; /* we only use 32 bit values, even though we're running in x64 */
#define ob(b) do { if (!C.scanningForLocals) *C.codep++ = (b); } while(0)
#define outnum(n) { uintptr_t _ = (uintptr_t)n; uintptr_t mask = 0xff; uintptr_t sh = 0; int i; \
    for (i = 0; i < REG_SIZE; ++i) { ob((_&mask)>>sh); mask <<= 8; sh += 8; } }
#define get32(p) (*(int*)p)

static void g_rval(int reg)
{
    if (zvlast(C.vst).type & V_CONST)
    {
        /* mov reg, const */
        ob(0xb8 + reg);
        outnum(zvlast(C.vst).data.i);
        zvpop(C.vst);
    }
    else if (zvlast(C.vst).type & V_CMP)
    {
        /* setxx */
        ob(0x0f);
        ob(0x90 + zvlast(C.vst).data.i);
        ob(0xc0 + reg);
        zvpop(C.vst);
    }
    else if (zvlast(C.vst).type == V_LVAL)
    {
        error("todo;");
    }
    else
    {
        error("internal error, unexpected stack state");
    }
}

static void g_prolog()
{
    ob(0x55); /* push rbp */
    ob(0x48); ob(0x89); ob(0xe5); /* mov rbp, rsp */
    VAL(V_CONST, 0); /* for fall off ret */
}

static void g_leave_ret()
{
    g_rval(R_0);
    ob(0xc9); /* leave */
    ob(0xc3); /* ret */
} 

#define put32(p, n) (*(int*)(p) = (n))

/* emit a jump, returns the location that needs to be fixed up. make a linked
 * list to previous items that are going to jump to the same final location so
 * that when the jump target is reached we can fix them all up by walking the
 * list that we created. */
static char* g_jmp(char* prev)
{
    ob(0xe9);
    outnum(prev);
    return C.codep - 4;
}

/* NZ is 0/1 for Z/NZ test. see note about prev above. */
static char* g_test(int NZ, char* prev)
{
    g_rval(0);
    ob(0x85); ob(0xc0); /* test eax, eax */
    ob(0x0f); ob(0x84 + NZ); /* jz/jnz rrr */
    outnum(prev);
    return C.codep - 4;
}


static void g_fixup1(char* p, char* to)
{
    if (C.scanningForLocals) return;
    while (p)
    {
        char* tmp = (char*)(uintptr_t)get32(p); /* next value in the list before we overwrite it */
        put32(p, to - p - 4);
        p = tmp;
    }
}

static void g_fixup(char* p) { g_fixup1(p, C.codep); }

/* compares the two tops of the stack and pushes the result flag */
static void g_cmp(int CC)
{
    g_rval(R_0);
    g_rval(R_1);
    ob(0x39); ob(0xc1); /* cmp ecx, eax */
    VAL(V_CMP, CC);
}

static void g_dup()
{
    ob(0x58); ob(0x50); ob(0x50); /* pop eax; push eax; push eax. todo; should be push [esp]? */
    zvpush(C.vst, zvlast(C.vst));
}

static void g_store()
{
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
        VAL(V_CONST, CURTOK.data.tokn);
        NEXT();
    }
    else if (CURTOKt == T_IDENT)
    {
        VAL(V_CONST, 0);
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
        atom();
        g_cmp(cmptoks[cmptoki].cc);
    }
}

static void not_test()
{
    if (CURTOKt == KW(not))
    {
        SKIP(KW(not));
        not_test();
    }
    else
        comparison();
}

static void and_test()
{
    not_test();
    while (CURTOKt == KW(and))
    {
        SKIP(KW(and));
        error("todo;");
        not_test();
    }
}

static void or_test()
{
    and_test();
    while (CURTOKt == KW(or))
    {
        SKIP(KW(or));
        error("todo;");
        and_test();
    }
}
static void expr_stmt()
{
    or_test();
    while (CURTOKt == '=')
    {
        NEXT();
        or_test();
        //if (CURTOKt == '=') g_dup();
        g_store();
    }
}

static void stmt()
{
    char *offdone = 0, *offtest = 0;
    if (CURTOKt == KW(return))
    {
        SKIP(KW(return));
        if (CURTOKt == T_DEDENT) VAL(V_CONST, 20710);
        else or_test();
        g_leave_ret();
    }
    else if (CURTOKt == KW(print))
    {
        SKIP(KW(print));
        NEXT();
    }
    else if (CURTOKt == KW(pass))
    {
        NEXT();
        /* nothing */
    }
    else if (CURTOKt == KW(if))
    {
        SKIP(KW(if));
        comparison();
        offtest = g_test(0, 0);
        suite();
        offdone = g_jmp(offdone);
        g_fixup(offtest);
        while (CURTOKt == KW(elif) || CURTOKt == KW(else))
        {
            NEXT();
            if (PREVTOK.type == KW(elif))
            {
                comparison();
                offtest = g_test(0, 0);
            }
            else offtest = 0;
            suite();
            if (offtest)
            {
                offdone = g_jmp(offdone);
                g_fixup(offtest);
            }
        }
        g_fixup(offdone);
    }
    else if (CURTOKt == T_NL) error("bad indent");
    else expr_stmt();
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
    int fstart;
    SKIP(KW(def));
    SKIP(T_IDENT);
    if (strcmp(PREVTOK.data.str, "__main__") == 0) C.entry = C.codep;
    SKIP('(');
    SKIP(')');

    fstart = C.curtok;
    C.scanningForLocals = 1;
    C.locals = zvfree(C.locals);
    suite();

    C.curtok = fstart;
    C.scanningForLocals = 0;

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
#if 0 /* dump tokens generated from stream */
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
#if 1 /* dump disassembly of generated code, needs ndisasm in path */
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
    zvfree(C.vst);
    zvfree(C.locals);
    zept_freeExec(C.codeseg, allocSize);
    return ret;
}

#endif /* ZEPT_DEFINE_IMPLEMENTATION */
