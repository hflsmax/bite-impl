
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
  void *factorial_fptr;
  void *factorialTail_fptr;
} main_locals_t;

typedef main_locals_t factorialRec_env_t;
typedef struct factorialRec_locals_t {
  factorialRec_env_t *env;
  int n;
  int acc;
  int factorialRec_env;
} factorialRec_locals_t;

typedef main_locals_t factorialTail_1_env_t;
typedef struct factorialTail_1_locals_t {
  factorialTail_1_env_t *env;
  int n;
} factorialTail_1_locals_t;
int factorialRec(factorialRec_env_t *env, int n, int acc);
int factorialTail_1(factorialTail_1_env_t *env, int n);
int main();
volatile void *factorial_env;
volatile void *factorialTail_env;

int factorialRec(factorialRec_env_t *env, int n, int acc) {
  factorialRec_locals_t locals;
  locals.env = env;
  locals.n = n;
  locals.acc = acc;

  locals.factorialRec_env = locals.env;
  if (({ locals.n == 0; })) {
    return locals.acc;
  } else {
    __attribute__((musttail)) return factorialRec(locals.factorialRec_env,
                                                  ({ locals.n - 1; }),
                                                  ({ locals.acc *locals.n; }));
  };
}

int factorialTail_1(factorialTail_1_env_t *env, int n) {
  factorialTail_1_locals_t locals;
  locals.env = env;
  locals.n = n;

  return factorialRec(factorial_env, locals.n, 1);
}

int main() {
  main_locals_t locals;

  locals.factorial_fptr = (void *)factorialRec;
  locals.factorialTail_fptr = (void *)factorialTail_1;
  return Print(factorialTail_1(factorialTail_env, 100100100));
}
