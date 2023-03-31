
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
  void *factorial_fptr;
  void *factorial_env;
  void *factorial_jb;
  void *factorialTail_fptr;
  void *factorialTail_env;
  void *factorialTail_jb;
} main_locals_t;

typedef main_locals_t factorialRec_env_t;
typedef struct factorialRec_locals_t {
  factorialRec_env_t *env;
  int n;
  int acc;
} factorialRec_locals_t;

typedef main_locals_t fn1_env_t;
typedef struct fn1_locals_t {
  fn1_env_t *env;
  int n;
} fn1_locals_t;

int factorialRec(factorialRec_env_t *env, int n, int acc) {
  factorialRec_locals_t locals;
  locals.env = env;
  locals.n = n;
  locals.acc = acc;

  if (({ locals.n == 0; })) {
    return locals.acc;
  } else {
    __attribute__((musttail)) return factorialRec(
        locals.env, ({ locals.n - 1; }), ({ locals.acc *locals.n; }));
  }
}

int fn1(fn1_env_t *env, int n) {
  fn1_locals_t locals;
  locals.env = env;
  locals.n = n;

  return factorialRec(locals.env->factorial_env, locals.n, 1);
}

int main() {
  main_locals_t locals;

  locals.factorial_fptr = (void *)factorialRec;
  locals.factorial_env = &locals;
  locals.factorialTail_fptr = (void *)fn1;
  locals.factorialTail_env = &locals;
  return Print(fn1(locals.factorialTail_env, 33));
}
