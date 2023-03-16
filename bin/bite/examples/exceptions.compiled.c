
#include <setjmp.h>

typedef struct closture_t {
    void *f_ptr;
    void *env;
    jmp_buf jb;
} closure_t;

closure_t copy_closure(closure_t from) {
    return from;
}

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
closure_t lexc;
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
locals.exch = (closure_t){exch, env};
locals.env = env;

jmpret = 0;
longjmp(jb, 1);

}

int g(void* env, int n)
{
g_locals_t locals;
locals.g = (closure_t){g, env};
locals.env = env;
locals.n = n;

locals.lexc = ({locals.exch.f_ptr = exch;
copy_closure(locals.exch);});
if (setjmp(locals.lexc.jb) == 0) {
if (({locals.n == 0;})) {
return ((int(*)(void*, jmp_buf))locals.lexc.f_ptr)(locals.lexc.env, locals.lexc.jb);
} else {
__attribute__((musttail))return ((int(*)(void*, int))locals.g.f_ptr)(locals.g.env, ({locals.n - 1;}));
};
} else {
return jmpret;
}
}

int main()
{
main_locals_t locals;

locals.run = ({locals.g.f_ptr = g;
locals.g.env = &locals;
copy_closure(locals.g);});
return ((int(*)(void*, int))locals.run.f_ptr)(locals.run.env, 42);
}
