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

void* arrayMalloc(int size) {
    return malloc(size * sizeof(int));
}

int arrayGet(void* arr, int index) {
    return ((int*)arr)[index];
}

typedef struct main_locals_t {
main_env_t* env;
int arrLen;
void* arr;
int iterIdx;
void *iterNext_fptr;
void *iterNext_env;
jmp_buf *iterNext_jb;
void *add_fptr;
void *add_env;
jmp_buf *add_jb;
void *foldLeft_fptr;
void *foldLeft_env;
jmp_buf *foldLeft_jb;
} main_locals_t;

typedef main_locals_t fn1_env_t;
typedef struct fn1_locals_t {
fn1_env_t* env;
void *exc_fptr;
void *exc_env;
jmp_buf *exc_jb;
} fn1_locals_t;

typedef main_locals_t fn2_env_t;
typedef struct fn2_locals_t {
fn2_env_t* env;
int a;
int b;
} fn2_locals_t;

typedef main_locals_t foldLeftRec_env_t;
typedef struct foldLeftRec_locals_t {
foldLeftRec_env_t* env;
jmp_buf fn3_jb;
void *op_fptr;
void *op_env;
jmp_buf *op_jb;
int acc;
int next;
bool toBreak;
void *exc_fptr;
void *exc_env;
jmp_buf *exc_jb;
} foldLeftRec_locals_t;

typedef foldLeftRec_locals_t fn3_env_t;
typedef struct fn3_locals_t {
fn3_env_t* env;
} fn3_locals_t;
int fn1(void* env, void *exc_fptr, void *exc_env, jmp_buf *exc_jb)
{
fn1_locals_t locals;
locals.env = (fn1_env_t*)env;
locals.exc_fptr = exc_fptr;
locals.exc_env = exc_env;
locals.exc_jb = exc_jb;

if (({locals.env->iterIdx < locals.env->arrLen;})) {

locals.env->iterIdx = ({locals.env->iterIdx + 1;});
return arrayGet(locals.env->arr, ({locals.env->iterIdx - 1;}));
} else {
return ((int(*)(void*, jmp_buf*))locals.exc_fptr)(locals.exc_env, locals.exc_jb);
}
}

int fn2(void* env, int a, int b)
{
fn2_locals_t locals;
locals.env = (fn2_env_t*)env;
locals.a = a;
locals.b = b;

return ({locals.a + locals.b;});
}

int fn3(void* env, jmp_buf jb)
{
fn3_locals_t locals;
locals.env = (fn3_env_t*)env;

locals.env->toBreak = true;
_longjmp(jb, 1);

}

int foldLeftRec(void* env, void* op_fptr, void* op_env, int acc)
{
foldLeftRec_locals_t locals;
locals.env = (foldLeftRec_env_t*)env;
locals.op_fptr = op_fptr;
locals.op_env = op_env;
locals.acc = acc;

locals.next = 0;
locals.toBreak = false;

({locals.exc_fptr = (void*)fn3;
locals.exc_env = &locals;
jmp_buf _exc_jb;
locals.exc_jb = &_exc_jb;
(_setjmp(*locals.exc_jb) == 0 ? ({locals.next = fn1(locals.env->iterNext_env, locals.exc_fptr, locals.exc_env, locals.exc_jb);}) : ({jmpret;}));});
if (locals.toBreak) {
return locals.acc;
} else {
__attribute__((musttail))return foldLeftRec(locals.env, locals.op_fptr, locals.op_env, ((int(*)(void*, int, int))locals.op_fptr)(locals.op_env, locals.acc, locals.next));
};
}

int main()
{
main_locals_t locals;

locals.arrLen = 100100100;
locals.arr = arrayMalloc(locals.arrLen);
locals.iterIdx = 0;
locals.iterNext_fptr = (void*)fn1;
locals.iterNext_env = &locals;
locals.add_fptr = (void*)fn2;
locals.add_env = &locals;
locals.foldLeft_fptr = (void*)foldLeftRec;
locals.foldLeft_env = &locals;
return foldLeftRec(locals.foldLeft_env, locals.add_fptr, locals.add_env, 0);
}
