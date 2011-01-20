#define TWOK_DEFINE_IMPLEMENTATION
#include "twok.h"
#include <stdio.h>

char testdata[1<<24];
char curtest[1<<24];
char description[256];

int XXXtest0(int a, int b) 
{
    return a - b;
}

int XXXtest1(int a, int b, int c, int d)
{
    return 1000*a + 100*b + 10*c + d;
}

int XXXtest2(int a, int b, int c, int d, int e, int f, int g, int h)
{
    int tmp = XXXtest1(a,b,c,d);
    int tmp2 = 10000*e + 100000*f + 1000000*g + 10000000*h;
    return tmp+tmp2;
}

long long XXXtest3(long long a, long long b, long long c, long long d, long long e, long long f, long long g, long long h)
{
    return (h-a)+(g-b)+(f-c)+(e-d) + 1;
}

void test5()
{
    printf("in test5");
}

int test6(void(*fp)())
{
    int ret = fp == test5;
    fp();
    return ret;
}

void* getExternFunc(char *name)
{
    if (strcmp(name, "test0") == 0) return XXXtest0;
    if (strcmp(name, "test1") == 0) return XXXtest1;
    if (strcmp(name, "test2") == 0) return XXXtest2;
    if (strcmp(name, "test3") == 0) return XXXtest3;
    if (strcmp(name, "test5") == 0) return test5;
    if (strcmp(name, "test6") == 0) return test6;
    return NULL;
}

void copyline(char** dest, char** src)
{
    int done;
    for (;;)
    {
        **dest = **src;
        done = **src == '\n';
        (*dest)++;
        (*src)++;
        if (done) return;
    }
}

int main(int argc, char** argv)
{
    int i = 0, ret, failCount = 0, passCount = 0, disabledCount = 0, failed, verbose = 0;
    int expectedRC;
    FILE* f = fopen("tests.twok", "rb");
    char* src = testdata;
    char* dest, *desc;
    ret = /* warning suppress */ (int)fread(src, 1, 1<<24, f);
    if (argc >= 2 && strcmp(argv[1], "-v") == 0)
    {
        argc--;
        argv++;
        verbose = 1;
    }
    for (;; ++i)
    {
        if (src[0] != '#' || src[1] != '#' || src[2] != '#')
        {
            fprintf(stderr, "expecting ### line in tests\n");
            exit(1);
        }
        src += 4;
        expectedRC = strtol(src, &src, 0);
        desc = description;
        src += 1;
        copyline(&desc, &src);
        *(desc - 1) = 0;
        if (strcmp(description, "END") == 0)
        {
            printf("%d/%d tests passed (+%d disabled)\n", passCount, passCount + failCount, disabledCount);
            break;
        }
        dest = curtest;
        while (!(src[0] == '#' && src[1] == '#' && src[2] == '#'))
            copyline(&dest, &src);
        *dest = 0;
        if ((argc == 2 && atoi(argv[1]) != i)
                || (argc == 3 && (atoi(argv[1]) > i || atoi(argv[2]) < i)))
                continue;
        if (strncmp(description, "DISABLED", 8) == 0)
        {
            disabledCount++;
            continue;
        }
        printf("[%3d %20s %s]: ", i, description, expectedRC == -1 ? "err" : "   ");
        ret = twokRun(curtest, getExternFunc);
        failed = ret != expectedRC || (expectedRC == -1 && strstr(C.errorText, description) == NULL);
        printf("%s\n", failed ? "FAILED": "ok");
        failCount += failed;
        passCount += !failed;
        if (failed || verbose)
        {
            printf("\n------------------------\n%s------------------------\n", curtest);
            printf("rc=%d, want=%d, desc='%s'\n%s%s\n\n", ret, expectedRC, description,
                    C.errorText[0] ? "Error:\n" : "", C.errorText);
        }
    }
    return failCount;
}
