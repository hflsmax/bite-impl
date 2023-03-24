
#include <stdio.h>
#include <setjmp.h>

typedef struct closture_t {
    void *f_ptr;
    void *env;
    jmp_buf jb;
} closure_t;

volatile int jmpret;

typedef struct main_env_t {} main_env_t;


typedef struct main_locals_t {
main_env_t* env;
closure_t main;
closure_t run;
closure_t g;
} main_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
g_env_t* env;
closure_t g;
int n;
closure_t *lexc;
closure_t exch;
} g_locals_t;

typedef g_locals_t exch_env_t;
typedef struct exch_locals_t {
exch_env_t* env;
closure_t exch;
} exch_locals_t;
int exch(void* env, jmp_buf jb)
{
exch_locals_t locals;
locals.exch = (closure_t){(void*)exch, env};
locals.env = (exch_env_t*)env;

printf("ohye\n");
jmpret = 0;
_longjmp(jb, 1);

}

int g(void* env, int n)
{
g_locals_t locals;
// locals.g = (closure_t){(void*)g, env};
locals.g.f_ptr = (void*)g;
locals.g.env = env;
locals.env = (g_env_t*)env;
locals.n = n;

locals.exch.f_ptr = (void*)exch;
locals.lexc = &locals.exch;
if (_setjmp(locals.lexc->jb) == 0) {
    if (({locals.n == 0;})) {
        return exch(locals.lexc->env, locals.lexc->jb);
    } else {
        __attribute__((musttail))return g(locals.g.env, ({locals.n - 1;}));
    };
} else {
    return jmpret;
}
}

int main()
{
main_locals_t locals;

locals.run = ({locals.g.f_ptr = (void*)g;
locals.g.env = &locals;
copy_closure(locals.g);});
printf("%d\n", g(locals.run.env, 10010010));
}
