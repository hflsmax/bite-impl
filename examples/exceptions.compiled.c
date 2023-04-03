
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
  void *run_fptr;
} main_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
  g_env_t *env;
  int n;
  void *lexc_fptr;
  void *lexc_env;
  void *lexc_jb;
} g_locals_t;

typedef g_locals_t lexc_1_env_t;
typedef struct lexc_1_locals_t {
  lexc_1_env_t *env;
  void *jb;
} lexc_1_locals_t;
int lexc_1(lexc_1_env_t *env, void *jb);
int g(g_env_t *env, int n);
int main();
void *run_env;

int lexc_1(lexc_1_env_t *env, void *jb) {
  lexc_1_locals_t locals;
  locals.env = env;
  locals.jb = jb;

  return 0;
}

int g(g_env_t *env, int n) {
  g_locals_t locals;
  locals.env = env;
  locals.n = n;

  locals.lexc_fptr = (void *)lexc_1;
  locals.lexc_jb = NULL;
  if (({ locals.n == 0; })) {
    return lexc_1(locals.lexc_env, locals.lexc_jb);
  } else {
    __attribute__((musttail)) return g(locals.env, ({ locals.n - 1; }));
  }
}

int main() {
  main_locals_t locals;

  locals.run_fptr = (void *)g;
  return Print(g(run_env, 100100100));
}
