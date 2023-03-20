
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

inline void* arrayMalloc(int size) {
    return malloc(size * sizeof(int));
}

inline int arrayGet(void* arr, int index) {
    return ((int*)arr)[index];
}


typedef struct main_locals_t {
main_env_t* env;
void *run_fptr;
void *run_env;
jmp_buf *run_jb;
} main_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
g_env_t* env;
jmp_buf fn1_jb;
int n;
void *lexc_fptr;
void *lexc_env;
jmp_buf *lexc_jb;
} g_locals_t;

typedef g_locals_t fn1_env_t;
typedef struct fn1_locals_t {
fn1_env_t* env;
} fn1_locals_t;
int fn1(void* env, jmp_buf jb)
{
fn1_locals_t locals;
locals.env = (fn1_env_t*)env;

jmpret = 0;
longjmp(jb, 1);

}

int g(void* env, int n)
{
g_locals_t locals;
locals.env = (g_env_t*)env;
locals.n = n;

locals.lexc_fptr = (void*)fn1;
locals.lexc_env = &locals;
jmp_buf _lexc_jb;
locals.lexc_jb = &_lexc_jb;
if (setjmp(*locals.lexc_jb) == 0) {
if (({locals.n == 0;})) {
return ((int(*)(void*, jmp_buf*))locals.lexc_fptr)(locals.lexc_env, locals.lexc_jb);
} else {
__attribute__((musttail))return g(locals.env, ({locals.n - 1;}));
};
} else {
return jmpret;
}
}

int main()
{
main_locals_t locals;

locals.run_fptr = (void*)g;
locals.run_env = &locals;
return g(locals.run_env, 10);

}
