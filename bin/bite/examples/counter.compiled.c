
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct closture_t {
    void *f_ptr;
    void *env;
    jmp_buf jb;
} closure_t;

volatile int jmpret;

typedef struct main_env_t {} main_env_t;

inline void* arrayInit(int size) {
    return malloc(size * sizeof(int));
}

inline int arrayGet(void* arr, int index) {
    return ((int*)arr)[index];
}


typedef struct main_locals_t {
main_env_t* env;
void *counter_fptr;
void *counter_env;
jmp_buf *counter_jb;
void *run_fptr;
void *run_env;
jmp_buf *run_jb;
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

typedef main_locals_t fn1_env_t;
typedef struct fn1_locals_t {
fn1_env_t* env;
jmp_buf fn2_jb;
jmp_buf fn3_jb;
int n;
int s;
void *lget_fptr;
void *lget_env;
jmp_buf *lget_jb;
void *lset_fptr;
void *lset_env;
jmp_buf *lset_jb;
} fn1_locals_t;

typedef fn1_locals_t fn2_env_t;
typedef struct fn2_locals_t {
fn2_env_t* env;
} fn2_locals_t;

typedef fn1_locals_t fn3_env_t;
typedef struct fn3_locals_t {
fn3_env_t* env;
int n;
} fn3_locals_t;
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

int fn2(void* env, jmp_buf jb)
{
fn2_locals_t locals;
locals.env = (fn2_env_t*)env;

return locals.env->s;
}

int fn3(void* env, jmp_buf jb, int n)
{
fn3_locals_t locals;
locals.env = (fn3_env_t*)env;
locals.n = n;

locals.env->s = locals.n;

}

int fn1(void* env, int n)
{
fn1_locals_t locals;
locals.env = (fn1_env_t*)env;
locals.n = n;

locals.s = locals.n;
locals.lget_fptr = (void*)fn2;
locals.lget_env = &locals;
jmp_buf _lget_jb;
locals.lget_jb = &_lget_jb;
locals.lset_fptr = (void*)fn3;
locals.lset_env = &locals;
jmp_buf _lset_jb;
locals.lset_jb = &_lset_jb;
return f(locals.env->counter_env, 0, locals.lget_fptr, locals.lget_env, locals.lget_jb, locals.lset_fptr, locals.lset_env, locals.lset_jb);
}

int main()
{
main_locals_t locals;

locals.counter_fptr = (void*)f;
locals.counter_env = &locals;
locals.run_fptr = (void*)fn1;
locals.run_env = &locals;
return fn1(locals.run_env, 10);


}
