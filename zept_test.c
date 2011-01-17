#define ZEPT_DEFINE_IMPLEMENTATION
#include "zept.h"
#include <stdio.h>

char testdata[1<<24];
char curtest[1<<24];
char description[256];

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

/*#define VERBOSE*/

int main(int argc, char** argv)
{
    int i = 0, ret, failCount = 0, passCount = 0, disabledCount = 0, failed;
    int expectedRC;
    FILE* f = fopen("tests.zept", "rb");
    char* src = testdata;
    char* dest, *desc;
    ret = /* warning suppress */ (int)fread(src, 1, 1<<24, f);
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
        ret = zeptRun(curtest);
        failed = ret != expectedRC || (expectedRC == -1 && strstr(C.errorText, description) == NULL);
        printf("%s\n", failed ? "FAILED": "ok");
        failCount += failed;
        passCount += !failed;
#ifdef VERBOSE
        /*if (failed)*/
        {
            printf("\n------------------------\n%s------------------------\n", curtest);
            printf("rc=%d, want=%d, desc='%s'\n%s%s\n\n", ret, expectedRC, description,
                    C.errorText[0] ? "Error:\n" : "", C.errorText);
        }
#endif
    }
    return failCount;
}
