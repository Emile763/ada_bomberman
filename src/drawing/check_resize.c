#include "check_resize.h"
#include <signal.h>
#include <stdatomic.h>
#ifndef SIGWINCH
#define SIGWINCH 28
#endif
static _Atomic int resized = 0;

static void sigwinchHandler(int code) {
    (void)code;
    resized = 1;
}

void initResizeHandler()
{
    signal (SIGWINCH, &sigwinchHandler);
}

int checkResized()
{
    if (resized)
    {
        resized = 0;
        return 1;
    }
    return 0;
}
