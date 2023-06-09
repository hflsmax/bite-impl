
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
  void *fibonacci_fptr;
} main_locals_t;

typedef main_locals_t fibonacciRec_env_t;
typedef struct fibonacciRec_locals_t {
  fibonacciRec_env_t *env;
  int n;
  int fibonacciRec_env;
} fibonacciRec_locals_t;
int fibonacciRec(fibonacciRec_env_t *env, int n);
int main();
volatile void *fibonacci_env;

int fibonacciRec(fibonacciRec_env_t *env, int n) {
  fibonacciRec_locals_t locals;
  locals.env = env;
  locals.n = n;

  locals.fibonacciRec_env = locals.env;
  if (({ locals.n == 0; })) {
    return 0;
  } else {
    if (({ locals.n == 1; })) {
      return 1;
    } else {
      return ({
        fibonacciRec(locals.fibonacciRec_env, ({ locals.n - 1; })) +
            fibonacciRec(locals.fibonacciRec_env, ({ locals.n - 2; }));
      });
    };
  };
}

int main() {
  main_locals_t locals;

  locals.fibonacci_fptr = (void *)fibonacciRec;
  return Print(fibonacciRec(fibonacci_env, 42));
}
