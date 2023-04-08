
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

#include "linkedList.h"

volatile int jmpret;

typedef struct main_env_t {
} main_env_t;

#define ArrayInitStatic(size) (&((int[size]){0}))

void *ArrayInit(int size) { return malloc(size * sizeof(int)); }

int ArrayGet(void *arr, int index) { return ((int *)arr)[index]; }

int Print(int x) {
  printf("%d\n", x);
  return 0;
}

typedef struct main_locals_t {
  void *counter_fptr;
  void *run_fptr;
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
  int f_env;
  int i;
} f_locals_t;

typedef main_locals_t run_1_env_t;
typedef struct run_1_locals_t {
  run_1_env_t *env;
  int n;
  int s;
  void *lget_fptr;
  void *lget_env;
  void *lget_jb;
  void *lset_fptr;
  void *lset_env;
  void *lset_jb;
} run_1_locals_t;

typedef run_1_locals_t lget_2_env_t;
typedef struct lget_2_locals_t {
  lget_2_env_t *env;
  void *jb;
} lget_2_locals_t;

typedef run_1_locals_t lset_3_env_t;
typedef struct lset_3_locals_t {
  lset_3_env_t *env;
  void *jb;
  int n;
} lset_3_locals_t;
int f(f_env_t *env, int n, void *lget_fptr, void *lget_env, void *lget_jb,
      void *lset_fptr, void *lset_env, void *lset_jb);
int lget_2(lget_2_env_t *env, void *jb);
void lset_3(lset_3_env_t *env, void *jb, int n);
int run_1(run_1_env_t *env, int n);
int main();
volatile void *counter_env;
volatile void *run_env;

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

  locals.f_env = locals.env;
  locals.i = ((int (*)(void *, void *))locals.lget_fptr)(locals.lget_env,
                                                         locals.lget_jb);
  if (({ locals.i == 0; })) {
    return locals.n;
  } else {

    ((void (*)(void *, void *, int))locals.lset_fptr)(
        locals.lset_env, locals.lset_jb, ({ locals.i - 1; }));
    __attribute__((musttail)) return f(
        locals.f_env, ({ locals.n + 1; }), locals.lget_fptr, locals.lget_env,
        locals.lget_jb, locals.lset_fptr, locals.lset_env, locals.lset_jb);
  };
}

int lget_2(lget_2_env_t *env, void *jb) {
  lget_2_locals_t locals;
  locals.env = env;
  locals.jb = jb;

  return locals.env->s;
}

void lset_3(lset_3_env_t *env, void *jb, int n) {
  lset_3_locals_t locals;
  locals.env = env;
  locals.jb = jb;
  locals.n = n;

  locals.env->s = locals.n;
}

int run_1(run_1_env_t *env, int n) {
  run_1_locals_t locals;
  locals.env = env;
  locals.n = n;

  locals.s = locals.n;
  locals.lget_fptr = (void *)lget_2;
  locals.lget_env = &locals;
  locals.lset_fptr = (void *)lset_3;
  locals.lset_env = &locals;
  return f(counter_env, 0, locals.lget_fptr, locals.lget_env, locals.lget_jb,
           locals.lset_fptr, locals.lset_env, locals.lset_jb);
}

int main() {
  main_locals_t locals;

  locals.counter_fptr = (void *)f;
  locals.run_fptr = (void *)run_1;
  return Print(run_1(run_env, 100100100));
}
