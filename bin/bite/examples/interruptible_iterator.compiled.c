#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "klist.h"

typedef struct closture_t {
    void *f_ptr;
    void *env;
    jmp_buf jb;
} closure_t;

volatile int jmpret;

typedef struct main_env_t {} main_env_t;

void* ArrayInit(int size) {
    return malloc(size * sizeof(int));
}

int ArrayGet(void* arr, int index) {
    return ((int*)arr)[index];
}

void noop(void* _) {}
KLIST_INIT(int_list, int, noop)

void* ListInit(int n) {
  klist_t(int_list)* list = (void*)kl_init(int_list);
  for (int i = 0; i < n; i++) {
    *kl_pushp(int_list, list) = i;
  }
  return list;
}

void ListPush(void* list, int i) {
  *kl_pushp(int_list, (klist_t(int_list)*)list) = i;
}

void ListShift(void* list) {
  kl_shift(int_list, (klist_t(int_list)*)list);
}

void* ListGetIter(void* list) {
  return kl_begin((klist_t(int_list)*)list);
}

void IterRemoveNext(void* iter) {
  kl_remove_next(int_list, iter);
}

bool IterHasNext(void* iter) {
  return ((kliter_t(int_list)*)iter)->next != NULL;
}

void* IterNext(void* iter) {
  return ((kliter_t(int_list)*)iter)->next;
}

void IterSet(void* iter, int val) {
  ((kliter_t(int_list)*)iter)->data = val;
}

int IterGet(void* iter) {
  return ((kliter_t(int_list)*)iter)->data;
}

typedef struct main_locals_t {
main_env_t* env;
jmp_buf fn3_jb;
jmp_buf fn4_jb;
void *iter_fptr;
void *iter_env;
jmp_buf *iter_jb;
void* list;
void *yield_fptr;
void *yield_env;
jmp_buf *yield_jb;
void *behead_fptr;
void *behead_env;
jmp_buf *behead_jb;
} main_locals_t;

typedef main_locals_t iterRec_env_t;
typedef struct iterRec_locals_t {
iterRec_env_t* env;
jmp_buf fn1_jb;
jmp_buf fn2_jb;
void* l;
void *yield_fptr;
void *yield_env;
jmp_buf *yield_jb;
void *behead_fptr;
void *behead_env;
jmp_buf *behead_jb;
void *replace_fptr;
void *replace_env;
jmp_buf *replace_jb;
void *localBehead_fptr;
void *localBehead_env;
jmp_buf *localBehead_jb;
} iterRec_locals_t;

typedef iterRec_locals_t fn1_env_t;
typedef struct fn1_locals_t {
fn1_env_t* env;
int x;
} fn1_locals_t;

typedef iterRec_locals_t fn2_env_t;
typedef struct fn2_locals_t {
fn2_env_t* env;
} fn2_locals_t;

typedef main_locals_t fn3_env_t;
typedef struct fn3_locals_t {
fn3_env_t* env;
int x;
void *replace_fptr;
void *replace_env;
jmp_buf *replace_jb;
void *behead_fptr;
void *behead_env;
jmp_buf *behead_jb;
} fn3_locals_t;

typedef main_locals_t fn4_env_t;
typedef struct fn4_locals_t {
fn4_env_t* env;
} fn4_locals_t;
int fn1(void* env, jmp_buf jb, int x)
{
fn1_locals_t locals;
locals.env = (fn1_env_t*)env;
locals.x = x;


IterSet(locals.env->l, locals.x);
return 0;
}

int fn2(void* env, jmp_buf jb)
{
fn2_locals_t locals;
locals.env = (fn2_env_t*)env;


IterRemoveNext(locals.env->l);
return 0;
}

int iterRec(void* env, void* l, void *yield_fptr, void *yield_env, jmp_buf *yield_jb, void *behead_fptr, void *behead_env, jmp_buf *behead_jb)
{
iterRec_locals_t locals;
locals.env = (iterRec_env_t*)env;
locals.l = l;
locals.yield_fptr = yield_fptr;
locals.yield_env = yield_env;
locals.yield_jb = yield_jb;
locals.behead_fptr = behead_fptr;
locals.behead_env = behead_env;
locals.behead_jb = behead_jb;


({locals.replace_fptr = (void*)fn1;
locals.replace_env = &locals;
jmp_buf _replace_jb;
locals.replace_jb = &_replace_jb;
((int(*)(void*, jmp_buf*, int, void*, void*, jmp_buf*, void*, void*, jmp_buf*))locals.yield_fptr)(locals.yield_env, locals.yield_jb, IterGet(locals.l), locals.replace_fptr, locals.replace_env, locals.replace_jb, locals.behead_fptr, locals.behead_env, locals.behead_jb);});
if (IterHasNext(locals.l)) {
return 0;
} else {
locals.localBehead_fptr = (void*)fn2;
locals.localBehead_env = &locals;
jmp_buf _localBehead_jb;
locals.localBehead_jb = &_localBehead_jb;
__attribute__((musttail))return iterRec(locals.env, IterNext(locals.l), locals.yield_fptr, locals.yield_env, locals.yield_jb, locals.localBehead_fptr, locals.localBehead_env, locals.localBehead_jb);
};
}

int fn3(void* env, jmp_buf jb, int x, void *replace_fptr, void *replace_env, jmp_buf *replace_jb, void *behead_fptr, void *behead_env, jmp_buf *behead_jb)
{
fn3_locals_t locals;
locals.env = (fn3_env_t*)env;
locals.x = x;
locals.replace_fptr = replace_fptr;
locals.replace_env = replace_env;
locals.replace_jb = replace_jb;
locals.behead_fptr = behead_fptr;
locals.behead_env = behead_env;
locals.behead_jb = behead_jb;

if (({locals.x < 0;})) {
return ((int(*)(void*, jmp_buf*))locals.behead_fptr)(locals.behead_env, locals.behead_jb);
} else {
return ((int(*)(void*, jmp_buf*, int))locals.replace_fptr)(locals.replace_env, locals.replace_jb, ({locals.x * 2;}));
}
}

int fn4(void* env, jmp_buf jb)
{
fn4_locals_t locals;
locals.env = (fn4_env_t*)env;


ListShift(locals.env->list);
return 0;
}

int main()
{
main_locals_t locals;

locals.iter_fptr = (void*)iterRec;
locals.iter_env = &locals;
locals.list = ListInit(100100);
locals.yield_fptr = (void*)fn3;
locals.yield_env = &locals;
jmp_buf _yield_jb;
locals.yield_jb = &_yield_jb;
locals.behead_fptr = (void*)fn4;
locals.behead_env = &locals;
jmp_buf _behead_jb;
locals.behead_jb = &_behead_jb;
return iterRec(locals.iter_env, locals.list, locals.yield_fptr, locals.yield_env, locals.yield_jb, locals.behead_fptr, locals.behead_env, locals.behead_jb);

}
