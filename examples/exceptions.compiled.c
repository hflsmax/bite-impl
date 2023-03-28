
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

typedef struct closture_t {
  void *f_ptr;
  void *env;
  jmp_buf jb;
} closure_t;

volatile int jmpret;

typedef struct main_env_t {
} main_env_t;

int Print(int x) {
  printf("%d\n", x);
  return 0;
}

typedef struct main_locals_t {
  main_env_t *env;
  void *run_fptr;
  void *run_env;
  jmp_buf *run_jb;
} main_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
  g_env_t *env;
  jmp_buf fn1_jb;
  int n;
  void *lexc_fptr;
  void *lexc_env;
  jmp_buf *lexc_jb;
} g_locals_t;

typedef g_locals_t fn1_env_t;
typedef struct fn1_locals_t {
  fn1_env_t *env;
} fn1_locals_t;
bool fn1_saved = false;
jmp_buf fn1_jb;
int fn1(void *env, jmp_buf jb) {
  fn1_locals_t locals;
  locals.env = (fn1_env_t *)env;

  jmpret = 0;
  _longjmp(jb, 1);
}

int g(void *env, int n) {
  g_locals_t locals;
  locals.env = (g_env_t *)env;
  locals.n = n;

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
  locals.run_env = &locals;
  return Print(g(locals.run_env, 100100100));
}
