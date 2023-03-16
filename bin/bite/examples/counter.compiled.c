
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
closure_t counter;
closure_t f;
closure_t run;
closure_t g;
} main_locals_t;

typedef main_locals_t f_env_t;
typedef struct f_locals_t {
f_env_t* env;
closure_t f;
int n;
closure_t lget;
closure_t lset;
int i;
} f_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
g_env_t* env;
closure_t g;
int n;
int s;
closure_t lget;
closure_t fget;
closure_t lset;
closure_t fset;
} g_locals_t;

typedef g_locals_t fget_env_t;
typedef struct fget_locals_t {
fget_env_t* env;
closure_t fget;
} fget_locals_t;

typedef g_locals_t fset_env_t;
typedef struct fset_locals_t {
fset_env_t* env;
closure_t fset;
int n;
} fset_locals_t;
int f(void* env, int n, closure_t lget, closure_t lset)
{
f_locals_t locals;
locals.f.f_ptr = (void*)f; locals.f.env = env;
locals.env = (f_env_t*)env;
locals.n = n;
locals.lget = lget;
locals.lset = lset;

locals.i = ((int(*)(void*, jmp_buf))locals.lget.f_ptr)(locals.lget.env, locals.lget.jb);
if (({locals.i == 0;})) {
return locals.n;
} else {

((int(*)(void*, jmp_buf, int))locals.lset.f_ptr)(locals.lset.env, locals.lset.jb, ({locals.i - 1;}));
return ((int(*)(void*, int, closure_t, closure_t))locals.f.f_ptr)(locals.f.env, ({locals.n + 1;}), locals.lget, locals.lset);
};
}

int fget(void* env, jmp_buf jb)
{
fget_locals_t locals;
locals.fget.f_ptr = (void*)fget; locals.fget.env = env;
locals.env = (fget_env_t*)env;

return locals.env->s;
}

int fset(void* env, jmp_buf jb, int n)
{
fset_locals_t locals;
locals.fset.f_ptr = (void*)fset; locals.fset.env = env;
locals.env = (fset_env_t*)env;
locals.n = n;

locals.env->s = locals.n;

}

int g(void* env, int n)
{
g_locals_t locals;
locals.g.f_ptr = (void*)g; locals.g.env = env;
locals.env = (g_env_t*)env;
locals.n = n;

locals.s = locals.n;
locals.lget = ({locals.fget.f_ptr = (void*)fget;
copy_closure(locals.fget);});
locals.lset = ({locals.fset.f_ptr = (void*)fset;
locals.fset.env = &locals;
copy_closure(locals.fset);});
return ((int(*)(void*, int, closure_t, closure_t))locals.env->counter.f_ptr)(locals.env->counter.env, 0, locals.lget, locals.lset);
}

int main()
{
main_locals_t locals;

locals.counter = ({locals.f.f_ptr = (void*)f;
locals.f.env = &locals;
copy_closure(locals.f);});
locals.run = ({locals.g.f_ptr = (void*)g;
locals.g.env = &locals;
copy_closure(locals.g);});
return ((int(*)(void*, int))locals.run.f_ptr)(locals.run.env, 10);
}
