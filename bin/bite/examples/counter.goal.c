
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
int n;

void *lget_fptr;
void *lget_env;
jmp_buf *lget_jb;

void *lset_fptr;
void *lset_env;
jmp_buf *lset_jb;

int i;
} f_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
g_env_t* env;

int n;
int s;

void *lget_fptr;
void *lget_env;
jmp_buf lget_jb;

void *lset_fptr;
void *lset_env;
jmp_buf lset_jb;
} g_locals_t;

typedef g_locals_t fget_env_t;
typedef struct fget_locals_t {
fget_env_t* env;
} fget_locals_t;

typedef g_locals_t fset_env_t;
typedef struct fset_locals_t {
fset_env_t* env;
int n;
} fset_locals_t;

int f(void* env, int n, void *lget_fptr, void *lget_env, jmp_buf *lget_jb, void *lset_fptr, void *lset_env, jmp_buf *lset_jb)
{
f_locals_t locals;
locals.env = (f_env_t*)env;
locals.n = n;
locals.lget_fptr = lget_fptr;
locals.lget_env = lget_env;
locals.lget_jb = lget_jb;
locals.lset_fptr = lset_fptr;
locals.lset_env = lset_env;
locals.lset_jb = lset_jb;

locals.i = ((int(*)(void*, jmp_buf*))locals.lget_fptr)(locals.lget_env, locals.lget_jb);
if (({locals.i == 0;})) {
return locals.n;
} else {

((int(*)(void*, jmp_buf*, int))locals.lset_fptr)(locals.lset_env, locals.lset_jb, ({locals.i - 1;}));
__attribute__((musttail))return f(locals.env, ({locals.n + 1;}), locals.lget_fptr, locals.lget_env, locals.lget_jb, locals.lset_fptr, locals.lset_env, locals.lset_jb);
};
}

int fget(void* env, jmp_buf jb)
{
fget_locals_t locals;
locals.env = (fget_env_t*)env;

return locals.env->s;
}

int fset(void* env, jmp_buf jb, int n)
{
fset_locals_t locals;
locals.env = (fset_env_t*)env;
locals.n = n;

locals.env->s = locals.n;

}

int g(void* env, int n)
{
g_locals_t locals;
locals.env = (g_env_t*)env;
locals.n = n;

locals.s = locals.n;
locals.lget_fptr = (void*)fget;
locals.lget_env = &locals;

locals.lset_fptr = (void*)fset;
locals.lset_env = &locals;

return f(locals.env->counter.env, 0, locals.lget_fptr, locals.lget_env, &locals.lget_jb, locals.lset_fptr, locals.lset_env, &locals.lset_jb);
}

int main()
{
main_locals_t locals;

locals.counter = ({locals.f.f_ptr = (void*)f;
copy_closure(locals.f);});
locals.run = ({locals.g.f_ptr = (void*)g;
locals.g.env = &locals;
copy_closure(locals.g);});
return g(locals.run.env, 10);
}
