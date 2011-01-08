#include "zept.h"
#include <stdio.h>

int main()
{
    int i;
    for (i = 0; ; ++i)
    {
        char buf[256];
        FILE* f;
        long len;
        char* p;
        sprintf(buf, "test%02d.zept", i);
        if ((f = fopen(buf, "rb")) != NULL)
        {
            fseek(f, 0, SEEK_END);
            len = ftell(f);
            rewind(f);
            p = malloc(len + 1);
            fread(p, 1, len, f);
            p[len] = 0;
            fclose(f);

            zept_run(p);
        }
        else
        {
            printf("%d tests run\n", i);
            break;
        }
    }
    return 0;
}
