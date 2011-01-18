/* zept-0.10 - public domain, python-ish script lang - http://h4ck3r.net/#Zept
   Scott Graham 2011 <scott.zept@h4ck3r.net>
                                    No warranty implied; use at your own risk.

Before including,

    #define ZEPT_DEFINE_IMPLEMENTATION

in *one* C file that you want to contain the implementation.

On Linux/Mac you need to link with libdl.a too (sorry.) Normally, this means
adding -ldl to the link command line somewhere.


ABOUT:

    Native compile on x64, ARM (not yet), or interpreted
    < 1k LOC (`sloccount zept.h`)
    No external dependencies

    ("zepto" is the SI prefix for 10e-21)


TODO/NOTES:

    function call args
    lists
    C function calls and runtime lib
    mempush/pop for 'gc'
    @var and free func for manual memory
    uninit var tracking (make it optional?)
    arm backend (on android ndk maybe)
    more tests for various math/expr ops

    dupe getReg calls in g_rval
    share genlocal and atom.T_IDENT


NOTES: (mostly internal mumbling)

    functions
        - indirected through global table for hotpatching
        - just add all interned names of functions to a list and use the index
          in that list as func identifier (hash won't work so well)
    need to know in body of function if name is a (global) function or not
        - if assigned (anywhere in the body) it's a local for the whole body
        - otherwise, it's a global function. only functions for now.
        - hmm, scan sucks. how about: global if already defined, otherwise local
          (works for funcs, except fwddecl, allow def f(): pass at some point)
    logic ops, and or:
        - allocate a stack tmp
        - store the value to check into tmp then branch if nz/z
        - reload stack for TOS at end of all or/ands
        - keeping reg alloc working is tricky because running through code in 
          straight line, so TOS doesn't mirror branching or bool ops. using
          stack means that the registers/vst aren't affected outside each arm
          of the or/and conditions.
    logical not: just == 0 and back into reg
    math functions, + - * / & | ^
    unary ops
    parens for precedence

    externs:
        - would be nice to dlsym externs automatically, but then we'd have to
          scan the body of functions to know what was assigned to, rather than
          just used.
        - we could either do that, or require explicit 'extern blah'
          declarations at global scope?
        - can't just dlsym first because then whatever was imported into the C
          program might override globals and locals of the program which would
          be stupid.
        - blech, dlsym sucks. can't get stuff from current elf unless you add
          -rdynamic to the command line. can load from clib or other .so, but,
          meh.


*/

#ifndef INCLUDED_ZEPT_H
#define INCLUDED_ZEPT_H

#ifdef __cplusplus
extern "C" {
#endif

extern int zeptRun(char *code, void *(*externLookup)(char *name));

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
#if __unix__ || (__APPLE__ && __MACH__)
    #include <sys/mman.h>
    #include <dlfcn.h>
    static void* zept_allocExec(int size) { void* p = mmap(0, size, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0); memset(p, 0x90, size); return p; }
    static void zept_freeExec(void* p, int size) { munmap(p, size); }
    static int zept_CTZ(int x) { return __builtin_ctz(x); }
#elif _WIN32
    #include <windows.h>
    static void* zept_allocExec(int size) { void* p = VirtualAlloc(0, size, MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE); memset(p, 0x90, size); return p; }
    static void zept_freeExec(void* p, int size) { VirtualFree(p, size, MEM_RELEASE); }
    #pragma intrinsic(_BitScanReverse)
    static int zept_CTZ(int x) { unsigned long ret; _BitScanReverse(&ret, x); return ret; }
    #define strdup _strdup
#endif

typedef struct Token {
    int type, pos;
    union {
        char* str;
        int tokn;
    } data;
} Token;

typedef struct Value {
    union {
        unsigned long _;
        int type;
        void (*handler)(struct Value*);
    } tag;
    union {
        unsigned long _;
        int i;
        char* p;
    } data;
    int label;
} Value;
#define VAL(t, d) do { Value _ = { { (t) }, { (d) }, 0xbad1abe1 }; zvpush(C.vst, _); } while(0)
#define J_UNCOND 2

typedef struct Context {
    Token *tokens;
    int curtok, irpos;
    char *input, *codeseg, *codesegend, *codep, **strs, **locals, **funcnames, **funcaddrs, **externnames, **externaddrs;
    void *(*externLookup)(char *name);
    Value *instrs, *vst;
    jmp_buf errBuf;
    char errorText[512];
} Context;


static Context C;
static void suite();
static void or_test();
static int atomplus();

/*
 * misc utilities.
 */

/* simple vector based on http://nothings.org/stb/stretchy_buffer.txt */
#define zvfree(a)                   ((a) ? (free(zv__zvraw(a)),(void*)0) : (void*)0)
#define zvpush(a,v)                 (zv__zvmaybegrow(a,1), (a)[zv__zvn(a)++] = (v))
#define zvpop(a)                    (assert(zv__zvn(a) > 0), zv__zvn(a)-=1)
#define zvsize(a)                   ((a) ? zv__zvn(a) : 0)
#define zvadd(a,n)                  (zv__zvmaybegrow(a,n), zv__zvn(a)+=(n), &(a)[zv__zvn(a)-(n)])
#define zvlast(a)                   ((a)[zv__zvn(a)-1])
#define zvindexofnp(a,i,n,psize)    (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),n,psize))
#define zvindexof(a,i)              ((a) ? (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),zv__zvn(a),sizeof(*(a)))) : -1)
#define zvcontainsnp(a,i,n,psize)   ((a) ? (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),n,psize)!=-1) : 0)
#define zvcontainsp(a,i,psize)      (zvcontainsnp((a),i,zv__zvn(a),psize))
#define zvcontainsn(a,i,n)          ((a) ? (zvcontainsnp((a),i,n,sizeof(*(a)))) : 0)
#define zvcontainsn_nonnull(a,i,n)  (zv__zvfind((char*)(a),(char*)&(i),sizeof(*(a)),n,sizeof(*(a)))!=-1) /* workaround for stupid warning */
#define zvcontains(a,i)             ((a) ? (zvcontainsp((a),i,sizeof(*(a)))) : 0)

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

static char KWS[] = " if elif else or for def return extern mod and not print pass << >> <= >= == != ";
#define KW(k) ((int)((strstr(KWS, #k " ") - KWS) + T_KW))
char* strintern(char* s)
{
    int i;
    for (i = 0; i < zvsize(C.strs); ++i)
        if (strcmp(s, C.strs[i]) == 0)
            return C.strs[i];
    zvpush(C.strs, strdup(s));
    return zvlast(C.strs);
}
enum { T_UNK, T_KW=1<<7, T_IDENT = 1<<8, T_END, T_NL, T_NUM, T_INDENT, T_DEDENT };
#define TOK(t) do { Token _ = { t, (int)(startpos - C.input), { strintern(#t) } }; zvpush(C.tokens, _); } while(0)
#define TOKI(t, s) do { Token _ = { t, (int)(startpos - C.input), { strintern(s) } }; zvpush(C.tokens, _); } while(0)
#define TOKN(t, v) do { Token _ = { t, (int)(startpos - C.input), { 0 } }; _.data.tokn=v; zvpush(C.tokens, _); } while(0)
#define isid(ch) (isalnum(ch) || ch == '_')

static void tokenize()
{
    char *pos = C.input, *startpos;
    int i, tok, column;
    int *indents = 0;
    char *ident = 0, *tempident = 0;

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
        while (column < zvlast(indents))
        {
            if (!zvcontains(indents, column)) error("unindent does not match any outer indentation level");
            zvpop(indents);
            TOK(T_DEDENT);
        }
        if (column > zvlast(indents))
        {
            zvpush(indents, column);
            TOK(T_INDENT);
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
                    /* oops, need to search with space before/after so "i"
                     * isn't found in "if" and "x" isn't found in "extern". */
                    zvpush(tempident, ' ');
                    for (i = 0; i < zvsize(ident); ++i) zvpush(tempident, ident[i]);
                    zvpush(tempident, ' ');
                    zvpush(tempident, 0);
                    tok = T_IDENT;
                    if (strstr(KWS, tempident)) tok = (int)(strstr(KWS, tempident) + 1 /*space*/ - KWS + T_KW);
                    TOKI(tok, ident);
                    tempident = zvfree(tempident);
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
        TOK(T_NL);
        ++pos;
    }
}

/*
 * backends, define one of them.
 */

/* x64 backend */
#if defined(_M_X64) || defined(__amd64__)

/* todo; add r8-r11? 0x48s need to change to 0x49 for that.
 * currently used: rax, rcx, rdx, rsi, rdi; all volatile across calls.
 * hmm. actually difficult to construct basic math ops that use more than 4
 * regs anyway, so not worth it straight away. */
enum { V_REG_RAX=0x0001, V_REG_RCX=0x0002, V_REG_RDX=0x0004, V_REG_RBX=0x0008,
       V_REG_RSP=0x0010, V_REG_RBP=0x0020, V_REG_RSI=0x0040, V_REG_RDI=0x0080,
       V_REG_ANY=V_REG_RAX | V_REG_RCX | V_REG_RDX | V_REG_RSI | V_REG_RDI,
       V_IMMED=0x0100, V_TEMP=0x0200, V_ADDR=0x0400, V_LOCAL=0x0800, V_FUNC=0x1000 };

/* bah. asshats used different abis for x64. only support 4 args for now since
 * windows only supports 4 w/o stack manip. */
enum {
#if __unix__ || (__APPLE__ && __MACH__)
    FUN_ARG0 = V_REG_RDI,
    FUN_ARG1 = V_REG_RSI,
    FUN_ARG2 = V_REG_RDX,
    FUN_ARG3 = V_REG_RCX,
#elif _WIN32
#endif
};


enum { REG_SIZE = 8, FUNC_THUNK_SIZE = 8 };

#define ob(b) (*C.codep++ = (b))
#define outnum32(n) { unsigned int _ = (unsigned int)(n); unsigned int mask = 0xff; unsigned int sh = 0; int i; for (i = 0; i < 4; ++i) { ob((char)((_&mask)>>sh)); mask <<= 8; sh += 8; } }
#define outnum64(n) { unsigned long _ = (unsigned long)(n); unsigned long mask = 0xff; unsigned long sh = 0; int i; for (i = 0; i < 8; ++i) { ob((char)((_&mask)>>sh)); mask <<= 8; sh += 8; } }

typedef struct NativeContext {
    int spills[64]; /* is the Nth spill location in use? */
    char* numlocsp;
} NativeContext;
static NativeContext NC;

#define vreg_to_enc(vr) zept_CTZ(vr)

#define put32(p, n) (*(int*)(p) = (n))
#define get32(p) (*(int*)p)

static char* functhunkaddr(int idx) { return C.codesegend - (idx + 1) * FUNC_THUNK_SIZE; }
static void addfunc(char* name, char* addr)
{
    char* p;
    if (zvcontains(C.funcnames, name)) error("%s already defined", name);
    zvpush(C.funcnames, name);
    zvpush(C.funcaddrs, addr);
    p = functhunkaddr(zvsize(C.funcnames) - 1);
    *p++ = 0xe9; /* jmp relimmed */
    put32(p, (int)(addr - p - 4)); p += 4;
    *p++ = 0xcc; *p++ = 0xcc; *p++ = 0xcc; /* add int3 to rest of thunk */
}
static int funcidx(char* name) { char* p = strintern(name); return zvindexof(C.funcnames, p); }

/* store the given register (offset) into the given stack slot */
static void g_store(int reg, int slot)
{
    (void)reg;
    (void)slot;
    error("todo; spill");
}

static int getReg(int valid)
{
    int i, j, reg;

    /* figure out if it's currently in use */
    for (i = V_REG_RAX; i <= V_REG_RDI; i <<= 1)
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
            for (i = 0; i < (int)(sizeof(NC.spills)/sizeof(NC.spills[0])); ++i)
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
    int reg, reg2, tag = zvlast(C.vst).tag.type, val = zvlast(C.vst).data.i;
    if (tag & V_IMMED)
    {
        if (tag & V_ADDR)
        {
            /* mov Reg, const */
            reg = getReg(valid);
            ob(0x48); ob(0xb8 + vreg_to_enc(reg));
            outnum64(val);
            /* mov [Reg], Reg */
            ob(0x48); ob(0x89);
            ob(vreg_to_enc(reg) + vreg_to_enc(reg) * 8);
        }
        else
        {
            /* mov Reg, const */
            reg = getReg(valid);
            ob(0x48); ob(0xb8 + vreg_to_enc(reg));
            outnum64(val);
        }
    }
    else if (tag & V_LOCAL)
    {
        /* todo; uninit var; keep shadow stack of initialized flags, error on
         * read before write. need to figure out calling C lib */
        reg = getReg(valid);
        ob(0x48); ob(0x8b); ob(0x85 + vreg_to_enc(reg) * 4); /* mov rXx, [rbp - xxx] (long form) */
        outnum32(-val * REG_SIZE - REG_SIZE);
    }
    else if (tag & V_FUNC)
    {
        reg = getReg(valid);
        ob(0x48); ob(0xb8 + vreg_to_enc(reg)); outnum64(functhunkaddr(val)); /* mov rXx, functhunk */
    }
    else if ((reg = (zvlast(C.vst).tag.type & V_REG_ANY) & valid)) { /* nothing to do, just return register */ }
    else if ((reg2 = (zvlast(C.vst).tag.type & V_REG_ANY)))
    {
        /* in a register, but not the one we need */
        int reg = getReg(valid);
        ob(0x48); ob(0x89); ob(0xc0 + vreg_to_enc(reg) + vreg_to_enc(reg2) * 8); /* mov rXx, rXx */
    }
    else
    {
        error("internal error, unexpected stack state");
    }
    zvpop(C.vst);
    return reg;
}

static int g_lval(int valid)
{
    int reg = 0, tag = zvlast(C.vst).tag.type, val = zvlast(C.vst).data.i;
    if (tag & V_ADDR)
    {
        if ((reg = (tag & V_REG_ANY))) { /* nothing, just pop and return reg */ }
        else if (tag & V_IMMED)
        {
            /* mov Reg, const */
            reg = getReg(valid);
            ob(0x48); ob(0xb8 + vreg_to_enc(reg));
            outnum64(val);
        }
        else if (tag & V_LOCAL)
        {
            reg = getReg(valid);
            ob(0x48); ob(0x8d); ob(0x85 + vreg_to_enc(reg) * 8); /* lea rXx, [rbp - xxx] (long form) */
            outnum32(-val * REG_SIZE - REG_SIZE);
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
static void i_addr(long v, int extra) { VAL(extra | V_ADDR, v); } /* extra is V_LOCAL, V_FUNC, V_IMMED */
static void i_func(Token* tok)
{
    while (((unsigned long)C.codep) % 16 != 0) ob(0x90); /* nop to align */
    addfunc(tok->data.str, C.codep);
    ob(0x55); /* push rbp */
    ob(0x48); ob(0x89); ob(0xe5); /* mov rbp, rsp */
    ob(0x48); ob(0x81); ob(0xec); outnum32(0); /* sub rsp, xxx */
    NC.numlocsp = C.codep - 4; /* save for endfunc to patch */
    VAL(V_IMMED, 0); /* for fall off ret */
}
static void i_extern(Token* tok)
{
    /* note, tok->data.str is already interned */
    void *p = 0;
    if (!p) error("'%s' not found", tok->data.str);
    if (zvcontains(C.externnames, tok->data.str)) return; /* not an error, just ignore. */
    zvpush(C.externnames, tok->data.str);
    zvpush(C.externaddrs, p);
}

static void i_ret() { g_rval(V_REG_RAX); ob(0xc9); /* leave */ ob(0xc3); /* ret */ }
static void i_endfunc()
{
    i_ret();
    put32(NC.numlocsp, (zvsize(C.locals)+1) * REG_SIZE + 256); /* todo; XXX hardcoded # spills */
}

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
    a = g_rval(V_REG_RAX);
    into = g_rval(V_REG_ANY & ~a);
    ob(0x48); ob(0x39 + vreg_to_enc(a)); ob(0xc0 + vreg_to_enc(into)); /* cmp eXx, eax */

    ob(0x48); ob(0xb8 + vreg_to_enc(into));
    outnum64(0);
    ob(0x0f);
    ob(0x90 + cmpccs[zvindexofnp(cmpccs, op, 6, 1)].cc);
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
        outnum32(prev ? prev - C.codeseg : 0);
    }
    else
    {
        int reg = g_rval(V_REG_ANY);
        ob(0x48); ob(0x85); ob(0xc0 + vreg_to_enc(reg) * 9); /* test rXx, rXx */
        ob(0x0f); ob(0x84 + cond); /* jz/jnz rrr */
        outnum32(prev ? prev - C.codeseg : 0);
    }
    return C.codep - 4;
}

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
    ob(0x48); ob(0x89);
    ob(vreg_to_enc(into) + vreg_to_enc(val) * 8);
}

static void i_storelocal(int loc)
{
    int val = g_rval(V_REG_ANY), into;
    i_addr(loc, V_LOCAL);
    into = g_lval(V_REG_ANY & ~val);
    ob(0x48); ob(0x89);
    ob(vreg_to_enc(into) + vreg_to_enc(val) * 8);
}

static void i_call(int argcount)
{
    /* todo; push args */
    (void)argcount;
    g_rval(V_REG_RAX);
    ob(0xff); ob(0xd0); /* call rax */
}

static void i_mathunary(int op)
{
    int reg;
    if (op == '+') return;
    reg = g_rval(V_REG_ANY);
    /* either neg or not */
    ob(0x48); ob(0xf7); ob(op == '-' ? 0xd8 : 0xd0 + vreg_to_enc(reg));
    VAL(reg, 0);
}

/* + - * / % & | ^ */
static void i_math(int op)
{
    struct { char math, opc; } map[] = { /* maps KW to x64 instr */
        { '+', 0x01 },
        { '-', 0x29 },
        { '*', 0x0f }, /* actually 0f af */
        { '&', 0x21 },
        { '^', 0x31 },
        { '|', 0x09 } };
    int opi = zvindexofnp(map, op, 6, 1);
    if (opi >= 0 || op == '*')
    {
        int v1 = g_rval(V_REG_ANY);
        int v0 = g_rval(V_REG_ANY & ~v1);
        ob(0x48); ob(map[opi].opc);
        if (op == '*') ob(0xaf);
        ob(0xc0 + vreg_to_enc(v0) + vreg_to_enc(v1) * 8);
        VAL(op == '*' ? v1 : v0, 0); /* bleh, extended imul args backwards? */
    }
    else if (op == '/' || op == '%')
    {
        int v1 = g_rval(V_REG_ANY & ~(V_REG_RAX | V_REG_RDX));
        g_rval(V_REG_RAX);
        ob(0x48); ob(0x99); /* cqo (sign extend rax into rdx) */
        ob(0x48); ob(0xf7); ob(0xf8 + vreg_to_enc(v1)); /* idiv rXx */
        VAL(op == '/' ? V_REG_RAX : V_REG_RDX, 0); /* quotient in A, remainder in D */
    }
}

#endif

/*
 * parsing and intermediate gen
 */
#define NEXT() do { if (C.curtok >= zvsize(C.tokens)) error("unexpected end of input"); C.curtok++; } while(0)
#define SKIP(t) do { if (CURTOKt != t) error("'%c' expected, got '%s'", t, CURTOK->data.str); NEXT(); } while(0)

static int genlocal()
{
    static int count = 0;
    char buf[128], *name;
    sprintf(buf, "$loc%d", count++);
    name = strintern(buf);
    if (!zvcontains(C.locals, name)) zvpush(C.locals, name);
    return zvindexof(C.locals, name);
}

static int atom()
{
    if (CURTOKt == '(')
    {
        NEXT();
        or_test();
        SKIP(')');
        return 1;
    }
    else if (CURTOKt == T_NUM)
    {
        i_const(CURTOK->data.tokn);
        NEXT();
        return 1;
    }
    else if (CURTOKt == T_IDENT)
    {
        int i;
        if ((i = zvindexof(C.externnames, CURTOK->data.str)) != -1) i_addr((unsigned long)C.externaddrs[i], V_IMMED);
        else if (funcidx(CURTOK->data.str) != -1) i_addr(funcidx(CURTOK->data.str), V_FUNC);
        else
        {
            char* name = strintern(CURTOK->data.str);
            if (!zvcontains(C.locals, name)) zvpush(C.locals, name);
            i_addr(zvindexof(C.locals, name), V_LOCAL);
        }
        NEXT();
        return 1;
    }
    return 0;
}

static int arglist()
{
    int count = atomplus();
    while (count && CURTOKt == ',')
    {
        SKIP(',');
        count += atomplus();
    }
    return count;
}

static int trailer()
{
    if (CURTOKt == '(')
    {
        int count;
        NEXT();
        count = arglist();
        SKIP(')');
        i_call(count);
        VAL(V_REG_RAX, -999); /* ret */
    }
    return 0;
}

static int atomplus()
{
    int ret = atom();
    while (trailer()) {}
    return ret;
}

static void factor()
{
    if (CURTOKt == '+' || CURTOKt == '-' || CURTOKt == '~')
    {
        int op = CURTOKt;
        NEXT();
        factor();
        i_mathunary(op);
    }
    else
    {
        atomplus();
    }
}

#define EXPRP(name, sub, tok0, tok1, tok2)                              \
static void name()                                                      \
{                                                                       \
    sub();                                                              \
    while (CURTOKt == tok0 || CURTOKt == tok1 || CURTOKt == tok2)       \
    {                                                                   \
        int op = CURTOKt;                                               \
        NEXT();                                                         \
        sub();                                                          \
        i_math(op);                                                     \
    }                                                                   \
}
EXPRP(term, factor, '*', '/', '%')
EXPRP(arith_expr, term, '+', '-', '-')
EXPRP(and_expr, arith_expr, '&', '&', '&')
EXPRP(xor_expr, and_expr, '^', '^', '^')
EXPRP(expr, xor_expr, '|', '|', '|')

static void comparison()
{
    char cmps[] = { '<', '>', KW(<=), KW(>=), KW(==), KW(!=) };
    expr();
    for (;;)
    {
        Token* cmp = CURTOK;
        if (!zvcontainsn_nonnull(cmps, CURTOKt, 6)) break;
        NEXT();
        expr();
        i_cmp(cmp->type);
    }
}

static void not_test()
{
    if (CURTOKt == KW(not))
    {
        SKIP(KW(not));
        comparison();
        VAL(V_IMMED, 0);
        i_cmp(KW(==));
    }
    else
        comparison();
}

#define BOOLOP(name, sub, kw, cond)             \
static void name()                              \
{                                               \
    char *label = 0;                            \
    sub();                                      \
    if (CURTOKt == KW(kw))                      \
    {                                           \
        int tmp = genlocal(), done = 0;         \
        for (;;)                                \
        {                                       \
            i_storelocal(tmp);                  \
            i_addr(tmp, V_LOCAL);               \
            label = i_jmpc(cond, label);        \
            if (done) break;                    \
            SKIP(KW(kw));                       \
            sub();                              \
            done = CURTOKt != KW(kw);           \
        }                                       \
        i_addr(tmp, V_LOCAL);                   \
    }                                           \
    i_label(label);                             \
}
BOOLOP(and_test, not_test, and, 0)
BOOLOP(or_test, and_test, or, 1)

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
    SKIP(T_NL);
}

static void stmt()
{
    char *labeldone = 0, *labeltest = 0;
    if (CURTOKt == KW(return))
    {
        SKIP(KW(return));
        if (CURTOKt == T_NL || CURTOKt == T_DEDENT) i_const(20710);
        else or_test();
        i_ret();
        SKIP(T_NL);
    }
    else if (CURTOKt == KW(print))
    {
        SKIP(KW(print));
        NEXT();
        SKIP(T_NL);
    }
    else if (CURTOKt == KW(pass))
    {
        NEXT();
        SKIP(T_NL);
    }
    else if (CURTOKt == KW(if))
    {
        SKIP(KW(if));
        or_test();

        labeltest = i_jmpc(0, 0);
        suite();
        labeldone = i_jmpc(J_UNCOND, 0);
        i_label(labeltest);

        while (CURTOKt == KW(elif) || CURTOKt == KW(else))
        {
            NEXT();
            if (PREVTOK.type == KW(elif))
            {
                or_test();
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
    if (CURTOKt == KW(extern))
    {
        SKIP(KW(extern));
        i_extern(CURTOK);
        NEXT();
        SKIP(T_NL);
    }
    else
    {
        SKIP(KW(def));
        zvfree(C.locals);
        i_func(CURTOK);
        SKIP(T_IDENT);
        SKIP('(');
        SKIP(')');
        suite();
        i_endfunc();
    }
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

/*
 * main api entry point
 */
int zeptRun(char *code, void *(*externLookup)(char *name))
{
    int ret, allocSize, i, entryidx;
    memset(&C, 0, sizeof(C));
    C.input = code;
    C.externLookup = externLookup;
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
        C.codesegend = C.codeseg + allocSize;
        fileinput();
        if (zvsize(C.vst) != 0) error("internal error, values left on stack");
        /* dump disassembly of generated code, needs ndisasm in path */
#if 0
        { FILE* f = fopen("dump.dat", "wb");
        fwrite(C.codeseg, 1, C.codep - C.codeseg, f);
        fclose(f);
        ret = system("ndisasm -b64 dump.dat"); }
#endif

        entryidx = funcidx("__main__");
        if (entryidx == -1) error("no entry point '__main__'");
        ret = ((int (*)())(C.codesegend - (entryidx + 1) * FUNC_THUNK_SIZE))();
    }
    else ret = -1;
    zvfree(C.tokens);
    zvfree(C.vst);
    zvfree(C.instrs);
    zvfree(C.locals);
    for (i = 0; i < zvsize(C.strs); ++i) free(C.strs[i]);
    zvfree(C.strs);
    zvfree(C.funcnames); zvfree(C.funcaddrs);
    zvfree(C.externnames); zvfree(C.externaddrs);
    zept_freeExec(C.codeseg, allocSize);
    return ret;
}

#endif /* ZEPT_DEFINE_IMPLEMENTATION */
