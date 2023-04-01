
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

void *ArrayInit(int size) { return malloc(size * sizeof(int)); }

int ArrayGet(void *arr, int index) { return ((int *)arr)[index]; }

int Print(int x) {
  printf("%d\n", x);
  return 0;
}

typedef struct main_locals_t {
  void *run_fptr;
  void *run_env;
  void *run_jb;
} main_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
  jmp_buf fn1_jb;
  g_env_t *env;
  int n;
  void *lexc_fptr;
  void *lexc_env;
  void *lexc_jb;
} g_locals_t;

typedef g_locals_t fn1_env_t;
typedef struct fn1_locals_t {
  fn1_env_t *env;
  void *jb;
} fn1_locals_t;
bool fn1_saved = false;
jmp_buf fn1_jb;

int fn1(fn1_env_t *env, void *jb) {
  fn1_locals_t locals;
  locals.env = env;
  locals.jb = jb;

  jmpret = 0;
  _longjmp(jb, 1);
}

int g(g_env_t *env, int n) {
  g_locals_t locals;
  locals.env = env;
  locals.n = n;

  locals.lexc_fptr = (void *)fn1;
  locals.lexc_jb = &fn1_jb;

  if (fn1_saved || _setjmp(locals.lexc_jb) == 0) {
    fn1_saved = true;
    if (({ locals.n == 0; })) {
      return fn1(locals.lexc_env, locals.lexc_jb);
    } else {
      __attribute__((musttail)) return g(locals.env, ({ locals.n - 1; }));
    };
  } else {
    return jmpret;
  }
}

int main() {
  main_locals_t locals;

  locals.run_fptr = (void *)g;
  return Print(g(locals.run_env, 100100100));
}
