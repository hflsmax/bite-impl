
typedef struct __jmp_buf_tag {
  unsigned long __jb[2];
} jmp_buf[1];
int setjmp(jmp_buf) __attribute__((__returns_twice__));
__attribute__((__noreturn__)) void longjmp(jmp_buf, int);

__asm__(".global _setjmp\n\t"
        ".global setjmp\n\t"
        ".type _setjmp, @function\n\t"
        ".type setjmp, @function\n\t"
        "_setjmp:\n\t"
        "setjmp:\n\t"
        "lea 8(%rsp),%rdx\n\t"
        "mov %rdx,(%rdi)\n\t"
        "mov (%rsp),%rdx\n\t"
        "mov %rdx,8(%rdi)\n\t"
        "xor %eax,%eax\n\t"
        "ret\n\t"

        ".global _longjmp\n\t"
        ".global longjmp\n\t"
        ".type _longjmp,@function\n\t"
        ".type longjmp,@function\n\t"
        "_longjmp:\n\t"
        "longjmp:\n\t"
        "xor %eax,%eax\n\t"
        "cmp $1,%esi\n\t"
        "adc %esi,%eax\n\t"
        "mov (%rdi),%rsp\n\t"
        "jmp *8(%rdi)\n\t");

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <mprompt.h>

#include "klist.h"

volatile int jmpret;

typedef struct main_env_t {
} main_env_t;

void *ArrayInit(int size) { return malloc(size * sizeof(int)); }

int ArrayGet(void *arr, int index) { return ((int *)arr)[index]; }

void noop(void *_) {}
KLIST_INIT(int_list, int, noop)

void *ListInit(int n) {
  klist_t(int_list) *list = (void *)kl_init(int_list);
  for (int i = 0; i < n; i++) {
    *kl_pushp(int_list, list) = i;
  }
  return list;
}

void ListPush(void *list, int i) {
  *kl_pushp(int_list, (klist_t(int_list) *)list) = i;
}

void ListShift(void *list) { kl_shift(int_list, (klist_t(int_list) *)list); }

void *ListGetIter(void *list) { return kl_begin((klist_t(int_list) *)list); }

void IterRemoveNext(void *iter) { kl_remove_next(int_list, iter); }

bool IterHasNext(void *iter) {
  return ((kliter_t(int_list) *)iter)->next != NULL;
}

void *IterNext(void *iter) { return ((kliter_t(int_list) *)iter)->next; }

void IterSet(void *iter, int val) { ((kliter_t(int_list) *)iter)->data = val; }

int IterGet(void *iter) { return ((kliter_t(int_list) *)iter)->data; }

int Print(int x) {
  printf("%d\n", x);
  return 0;
}

typedef struct main_locals_t {
  void *counter_fptr;
  void *counter_env;
  void *counter_jb;
  void *run_fptr;
  void *run_env;
  void *run_jb;
} main_locals_t;

typedef main_locals_t f_env_t;
typedef struct f_locals_t {
  f_env_t *env;
  int n;
  void *lget_fptr;
  void *lget_env;
  void *lget_jb;
  void *lset_fptr;
  void *lset_env;
  void *lset_jb;
  int i;
} f_locals_t;

typedef main_locals_t fn1_env_t;
typedef struct fn1_locals_t {
  jmp_buf fn2_jb;
  jmp_buf fn3_jb;
  fn1_env_t *env;
  int n;
  int s;
  void *lget_fptr;
  void *lget_env;
  void *lget_jb;
  void *lset_fptr;
  void *lset_env;
  void *lset_jb;
} fn1_locals_t;

typedef fn1_locals_t fn2_env_t;
typedef struct fn2_locals_t {
  fn2_env_t *env;
  void *jb;
} fn2_locals_t;

typedef fn1_locals_t fn3_env_t;
typedef struct fn3_locals_t {
  fn3_env_t *env;
  void *jb;
  int n;
} fn3_locals_t;

int f(f_env_t *env, int n, void *lget_fptr, void *lget_env, void *lget_jb,
      void *lset_fptr, void *lset_env, void *lset_jb) {
  f_locals_t locals;
  locals.env = env;
  locals.n = n;
  locals.lget_fptr = lget_fptr;
  locals.lget_env = lget_env;
  locals.lget_jb = lget_jb;
  locals.lset_fptr = lset_fptr;
  locals.lset_env = lset_env;
  locals.lset_jb = lset_jb;

  locals.i = ((int (*)(void *, void *))locals.lget_fptr)(locals.lget_env,
                                                         locals.lget_jb);
  if (({ locals.i == 0; })) {
    return locals.n;
  } else {

    ((int (*)(void *, void *, int))locals.lset_fptr)(
        locals.lset_env, locals.lset_jb, ({ locals.i - 1; }));
    __attribute__((musttail)) return f(
        locals.env, ({ locals.n + 1; }), locals.lget_fptr, locals.lget_env,
        locals.lget_jb, locals.lset_fptr, locals.lset_env, locals.lset_jb);
  };
}

int fn2(fn2_env_t *env, void *jb) {
  fn2_locals_t locals;
  locals.env = env;
  locals.jb = jb;

  return locals.env->s;
}

int fn3(fn3_env_t *env, void *jb, int n) {
  fn3_locals_t locals;
  locals.env = env;
  locals.jb = jb;
  locals.n = n;

  locals.env->s = locals.n;
}

int fn1(fn1_env_t *env, int n) {
  fn1_locals_t locals;
  locals.env = env;
  locals.n = n;

  locals.s = locals.n;
  locals.lget_fptr = (void *)fn2;
  locals.lget_env = &locals;
  locals.lset_fptr = (void *)fn3;
  locals.lset_env = &locals;
  return f(locals.env->counter_env, 0, locals.lget_fptr, locals.lget_env,
           locals.lget_jb, locals.lset_fptr, locals.lset_env, locals.lset_jb);
}

int main() {
  main_locals_t locals;

  locals.counter_fptr = (void *)f;
  locals.counter_env = &locals;
  locals.run_fptr = (void *)fn1;
  locals.run_env = &locals;
  return Print(fn1(locals.run_env, 100100100));
}
