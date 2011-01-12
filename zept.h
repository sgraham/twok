/* zept-0.10 - public domain, python-ish script lang - http://h4ck3r.net/#Zept
   Scott Graham 2011 <scott.zept@h4ck3r.net>
                                    No warranty implied; use at your own risk.

Before including,

    #define ZEPT_DEFINE_IMPLEMENTATION

in the file that you want to have the implementation.


ABOUT:

    Native compile on x64, ARM (not yet), or interpreted
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
#include <assert.h>
#include <setjmp.h>
#include <string.h>
#include <ctype.h>

#ifdef __unix__
    #include <stdint.h>
#else
    #include <stddef.h>
#endif

typedef struct Token {
    int type, pos;
    union {
        char str[32];
        int tokn;
    } data;
} Token;

typedef struct Value {
    union {
        uintptr_t __unused;
        int type;
        void (*handler)(struct Value*);
    } tag;
    union {
        uintptr_t __unused;
        int i;
        char* p;
    } data;
    int label;
} Value;
#define VAL(t, d) do { Value _ = { { (t) }, { (d) } }; zvpush(C.vst, _); } while(0)
#define J_UNCOND 2

typedef struct Context {
    Token* tokens;
    int curtok, irpos;
    char *input, *codeseg, *codep, *entry;
    Value *instrs, *vst;
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
#define zvadd(a,n)                  (zv__zvmaybegrow(a,n), zv__zvn(a)+=(n), &(a)[zv__zvn(a)-(n)])
#define zvlast(a)                   ((a)[zv__zvn(a)-1])
#define zvfindnp(a,i,n,psize)       (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),n,psize))
#define zvcontainsnp(a,i,n,psize)   (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),n,psize)!=-1)
#define zvcontainsp(a,i,psize)      (zvcontainsnp((a),i,zv__zvn(a),psize))
#define zvcontainsn(a,i,n)          (zvcontainsnp((a),i,n,sizeof(*(a))))
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
#define CURTOK (&C.tokens[C.curtok])
#define CURTOKt (CURTOK->type)

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
        geterrpos(CURTOK->pos, &line, &col, &text);
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

static char KWS[] = " if elif else or for def return mod and not print pass << >> <= >= == != ";
#define KW(k) ((int)((strstr(KWS, #k " ") - KWS) + T_KW))
enum { T_UNK, T_KW=1<<7, T_IDENT = 1<<8, T_END, T_NL, T_NUM, T_INDENT, T_DEDENT };
static Token* tokSetStr(Token* t, char* str)
{
    if (strlen(str) >= sizeof(t->data.str)) error("identifer too long");
    strcpy(t->data.str, str);
    t->data.str[sizeof(t->data.str) - 1] = 0;
    return t;
}
#define TOK(t) do { Token _ = { t, (int)(startpos - C.input) }; zvpush(C.tokens, *tokSetStr(&_, #t)); } while(0)
#define TOKI(t, s) do { Token _ = { t, (int)(startpos - C.input) }; zvpush(C.tokens, *tokSetStr(&_, s)); } while(0)
#define TOKN(t, v) do { Token _ = { t, (int)(startpos - C.input) }; _.data.tokn=v; zvpush(C.tokens, _); } while(0)
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
                    if (strstr(KWS, ident)) tok = (int)(strstr(KWS, ident) - KWS + T_KW);
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
                char tmp[3] = { 0, 0, 0 };
                if (!*pos) goto donestream;
                tmp[0] = *pos++;
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
 * backends, define one of them.
 */

#if 0

/* diagnostic 'backend', just prints out IR but doesn't run it */

static int IRpos;
static void i_const(int v) { printf("%5d: const %d\n", IRpos++, v); }
static void i_func(Token* tok) { printf("%5d: func %s\n", IRpos++, tok->data.str); }
static void i_ret() { printf("%5d: ret\n", IRpos++); }
static void i_cmp(int op) { printf("%5d: cmp %c%c\n", IRpos++, op < T_KW ? op : KWS[op-T_KW], op < T_KW ? ' ' : KWS[op-T_KW+1]); }
static char* i_jmpc(int cond, char* prev)
{
    printf("%5d: jmpc %s ->%p\n", IRpos++, cond == 0 ? "false" : (cond == 1 ? "true" : "uncond"), prev);
    return (char*)(uintptr_t)(IRpos - 1);
}
static void i_label(char* lab)
{
    uintptr_t li = (uintptr_t)lab;
    printf("       fixup to here at %ld\n", li);
}
/*static void i_math(Value* v) { printf("%5d: math\n", C.irpos); }*/

#elif defined(_M_X64) || defined(__amd64__)

/* x64 backend. word size is 32 bit. */

#define V_IMMED     0x01
#define V_REG_EAX   0x02
#define V_REG_ECX   0x04
#define V_REG_EDX   0x08
#define V_REG_EBX   0x10
#define V_REG_ANY (V_REG_EAX | V_REG_ECX | V_REG_EDX | V_REG_EBX)
#define V_TEMP      0x20
#define V_ADDR      0x40

enum { REG_SIZE = 4 }; /* we only use 32 bit values, even though we're running in x64 */
#define ob(b) (*C.codep++ = (b))
#define outnum(n) { uintptr_t _ = (uintptr_t)(n); uintptr_t mask = 0xff; uintptr_t sh = 0; int i; \
    for (i = 0; i < REG_SIZE; ++i) { ob((char)((_&mask)>>sh)); mask <<= 8; sh += 8; } }

typedef struct NativeContext {
    int spills[64]; /* is the Nth spill location in use? */
} NativeContext;
static NativeContext NC;

static int RegCatToRegOffset[5] = { 0, 1, 2, -999, 4 };
#define vreg_to_enc(vr) RegCatToRegOffset[((vr) >> 2)]

/* store the given register (offset) into the given stack slot */
static void g_store(int reg, int slot)
{
    error("todo; spill");
}

static int getReg(int valid)
{
    int i, j, reg;

    /* figure out if it's currently in use */
    for (i = V_REG_EAX; i <= V_REG_EBX; i <<= 1)
    {
        if ((i & valid) == 0) continue;
        for (j = 0; j < zvsize(C.vst); ++j)
            if ((C.vst[j].tag.type & i))
                break;
        /* not in use, return this one */
        if (j == zvsize(C.vst))
            return i;
    }

    /* otherwise, find the oldest in the class */
    for (j = 0; j < zvsize(C.vst); ++j)
    {
        if ((C.vst[j].tag.type & valid))
        {
            /* and a location to spill it to */
            for (i = 0; i < sizeof(NC.spills)/sizeof(NC.spills[0]); ++i)
                if (!NC.spills[i]) break;

            /* and send it there and update the flags */
            reg = C.vst[j].tag.type & valid;
            g_store(reg, i);
            NC.spills[i] = 1;
            C.vst[j].tag.type &= ~V_REG_ANY;
            C.vst[j].tag.type |= V_TEMP;
            C.vst[j].data.i = i;

            /* and return that register */
            return reg;
        }
    }
    error("internal error, or out of stack slots");
    return -1;
}

static int g_rval(int valid)
{
    int reg;
    if (zvlast(C.vst).tag.type & V_IMMED)
    {
        if (zvlast(C.vst).tag.type & V_ADDR)
        {
            /* mov reg, const */
            reg = getReg(valid);
            ob(0xb8 + vreg_to_enc(reg));
            outnum(zvlast(C.vst).data.i);
            /* mov [reg], reg */
            ob(0x67); ob(0x89);
            ob(vreg_to_enc(reg) + vreg_to_enc(reg) * 8);
            zvpop(C.vst);
        }
        else
        {
            /* mov reg, const */
            reg = getReg(valid);
            ob(0xb8 + vreg_to_enc(reg));
            outnum(zvlast(C.vst).data.i);
            zvpop(C.vst);
        }
    }
    else if (zvlast(C.vst).tag.type == V_ADDR)
    {
    }
    else if ((reg = (zvlast(C.vst).tag.type & V_REG_ANY) & V_REG_ANY))
    {
        return reg;
    }
    else
    {
        /* todo; reg-reg move */
        error("internal error, unexpected stack state");
    }
    return reg;
}

static int g_lval(int valid)
{
    int reg = 0;
    if (zvlast(C.vst).tag.type & V_ADDR)
    {
        if ((reg = (zvlast(C.vst).tag.type & V_REG_ANY))) { /* nothing, just pop and return reg */ }
        else if (zvlast(C.vst).tag.type & V_IMMED)
        {
            /* mov reg, const */
            reg = getReg(valid);
            ob(0xb8 + vreg_to_enc(reg));
            outnum(zvlast(C.vst).data.i);
        }
    }
    else
    {
        error("expecting lval");
    }
    zvpop(C.vst);
    return reg;
}

static void i_const(int v) { VAL(V_IMMED, v); }
static void i_func(Token* tok) {
    if (strcmp(tok->data.str, "__main__") == 0) C.entry = C.codep;
    ob(0x55); /* push rbp */
    ob(0x48); ob(0x89); ob(0xe5); /* mov rbp, rsp */
    ob(0x48); ob(0x81); ob(0xec); outnum(256); /* sub rsp, 100 */ /* todo; XXX hardcoded 64 locals */
    VAL(V_IMMED, 0); /* for fall off ret */
}

static void i_ret() { g_rval(V_REG_EAX); ob(0xc9); /* leave */ ob(0xc3); /* ret */ }

static void i_cmp(int op)
{
    int a, into;
    struct { char kw, cc; } cmpccs[] = {
        { '<', 0xc },
        { '>', 0xf },
        { KW(<=), 0xe },
        { KW(>=), 0xd },
        { KW(==), 4 },
        { KW(!=), 5 },
    };
    a = g_rval(V_REG_EAX);
    into = g_rval(V_REG_ANY & ~a);
    ob(0x39 + vreg_to_enc(a)); ob(0xc0 + vreg_to_enc(into)); /* cmp eXx, eax */

    ob(0xb8 + vreg_to_enc(into));
    outnum(0);
    ob(0x0f);
    ob(0x90 + cmpccs[zvfindnp(cmpccs, op, 6, 1)].cc);
    ob(0xc0 + vreg_to_enc(into));
    VAL(into, 0);
}

/* emit a jump, returns the location that needs to be fixed up. make a linked
 * list to previous items that are going to jump to the same final location so
 * that when the jump target is reached we can fix them all up by walking the
 * list that we created. */
static char* i_jmpc(int cond, char* prev)
{
    if (cond == J_UNCOND)
    {
        ob(0xe9);
        outnum(prev ? prev - C.codeseg : 0);
    }
    else
    {
        int reg = g_rval(V_REG_ANY);
        ob(0x85); ob(0xc0 + vreg_to_enc(reg) * 9); /* test eXx, eXx */
        ob(0x0f); ob(0x84 + cond); /* jz/jnz rrr */
        outnum(prev ? prev - C.codeseg : 0);
    }
    return C.codep - 4;
}

#define put32(p, n) (*(int*)(p) = (n))
#define get32(p) (*(int*)p)
static void i_label(char* p)
{
    char* to = C.codep;
    while (p)
    {
        char* tmp = get32(p) ? get32(p) + C.codeseg : 0; /* next value in the list before we overwrite it */
        put32(p, (int)(to - p - 4));
        p = tmp;
    }
}

/* lhs, rhs on stack */
static void i_store()
{
    int val = g_rval(V_REG_ANY);
    int into = g_lval(V_REG_ANY & ~val);
    ob(0x67); ob(0x89);
    ob(vreg_to_enc(into) + vreg_to_enc(val) * 8);
}

/*static void i_math(Value* v) {} */

#endif

/*
 * parsing and intermediate gen
 */
#define NEXT() do { if (C.curtok >= zvsize(C.tokens)) error("unexpected end of input"); C.curtok++; } while(0)
#define SKIP(t) do { if (CURTOKt != t) error("'%c' expected, got '%s'", t, CURTOK->data.str); NEXT(); } while(0)

#define INSTR2(i, d, d2) ((void)zvadd(C.instrs, 1), (void)((&zvlast(C.instrs))->tag.__unused = (uintptr_t)i), (void)((&zvlast(C.instrs))->data.__unused = (uintptr_t)d), (void)((&zvlast(C.instrs))->label = d2), zvsize(C.instrs) - 1)
#define INSTR1(i, d) (void)INSTR2(i, d, 0xbad1abe1)
#define INSTR0(i) (void)INSTR2(i, 0, 0)

static void atom()
{
    if (CURTOKt == T_NUM)
    {
        i_const(CURTOK->data.tokn);
        NEXT();
    }
    else if (CURTOKt == T_IDENT)
    {
        VAL(V_ADDR | V_IMMED, 0x1234);
        NEXT();
    }
    else error("unexpected atom");
}

static void comparison()
{
    char cmps[] = { '<', '>', KW(<=), KW(>=), KW(==), KW(!=) };
    atom();
    for (;;)
    {
        Token* cmp = CURTOK;
        if (!zvcontainsn(cmps, CURTOKt, 6)) break;
        NEXT();
        atom();
        i_cmp(cmp->type);
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
    if (CURTOKt == '=')
    {
        NEXT();
        or_test();
        i_store();
    }
    else zvpop(C.vst); /* discard */
}

static void stmt()
{
    char *labeldone = 0, *labeltest = 0;
    if (CURTOKt == KW(return))
    {
        SKIP(KW(return));
        if (CURTOKt == T_DEDENT) i_const(20710);
        else or_test();
        i_ret();
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

        labeltest = i_jmpc(0, 0);
        suite();
        labeldone = i_jmpc(J_UNCOND, 0);
        i_label(labeltest);

        while (CURTOKt == KW(elif) || CURTOKt == KW(else))
        {
            NEXT();
            if (PREVTOK.type == KW(elif))
            {
                comparison();
                labeltest = i_jmpc(0, 0);
            }
            else labeltest = 0;
            suite();
            if (labeltest)
            {
                labeldone = i_jmpc(J_UNCOND, labeldone);
                i_label(labeltest);
            }
        }
        i_label(labeldone);
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
    SKIP(KW(def));
    i_func(CURTOK);
    SKIP(T_IDENT);
    SKIP('(');
    SKIP(')');
    suite();
    i_ret();
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
    static void* zept_allocExec(int size) { return VirtualAlloc(0, size, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE); }
    static void zept_freeExec(void* p, int size) { VirtualFree(p, size, MEM_RELEASE); }
#endif


/*
 * main api entry point
 */
int zeptRun(char* code)
{
    int ret, allocSize;
    memset(&C, 0, sizeof(C));
    C.input = code;
    allocSize = 1<<17;
    if (setjmp(C.errBuf) == 0)
    {
        tokenize();
        /* dump tokens generated from stream */
#if 0
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

        /* dump disassembly of generated code, needs ndisasm in path */
#if 1
        { FILE* f = fopen("dump.dat", "wb");
        fwrite(C.codeseg, 1, C.codep - C.codeseg, f);
        fclose(f);
        ret = system("ndisasm -b64 dump.dat"); }
#endif

        if (!C.entry) error("no entry point '__main__'");
        ret = ((int (*)())C.entry)();
    }
    else
        ret = -1;
    zvfree(C.tokens);
    zvfree(C.vst);
    zvfree(C.instrs);
    zept_freeExec(C.codeseg, allocSize);
    return ret;
}

#endif /* ZEPT_DEFINE_IMPLEMENTATION */
