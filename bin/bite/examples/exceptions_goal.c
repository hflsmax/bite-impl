#include <setjmp.h>

typedef struct closture_t {
    void *f_ptr;
    void *env;
    jmp_buf jb;
} closure_t;

typedef struct main_env_t {} main_env_t;
typedef struct main_locals_t {
} main_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
g_env_t* env;
closure_t g;
int n;
int s;
closure_t lexc;
} g_locals_t;

volatile int jmpret;


int exch(void* env, jmp_buf jb) {
    jmpret = 42;
    longjmp(jb, 1);
}

int g(int n) {
    g_locals_t locals;
    locals.lexc.f_ptr = exch;
    locals.lexc.env = &locals;
    return ({
        (!setjmp(locals.lexc.jb)) ? (
            (n == 0) ? (
                ((int(*)(void*,jmp_buf))locals.lexc.f_ptr)(locals.lexc.env, locals.lexc.jb)
            ) : (
                g(n - 1)
            )
        ) : (
            jmpret
        );
    });
}

int main() {
    return g(10);
}