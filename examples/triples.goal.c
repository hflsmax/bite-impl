
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

#include "klist.h"

typedef struct closture_t {
  void *f_ptr;
  void *env;
  jmp_buf jb;
} closure_t;

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
  main_env_t *env;
  int cnt;
  void *countTriples_fptr;
  void *countTriples_env;
  jmp_buf *countTriples_jb;
} main_locals_t;

typedef main_locals_t fn1_env_t;
typedef struct fn1_locals_t {
  fn1_env_t *env;
  int n;
  int s;
  void *lch_fptr;
  void *lch_env;
  jmp_buf *lch_jb;
  int ra;
  int rb;
  int rc;
} fn1_locals_t;

typedef main_locals_t fn2_body_wrapper_env_t;
typedef struct fn2_body_wrapper_locals_t {
  fn2_body_wrapper_env_t *env;
} fn2_body_wrapper_locals_t;
int fn1(void *env, int n, int s, void *lch_fptr, void *lch_env,
        void *lch_jb) {
  fn1_locals_t locals;
  locals.env = (fn1_env_t *)env;
  locals.n = n;
  locals.s = s;
  locals.lch_fptr = lch_fptr;
  locals.lch_env = lch_env;
  locals.lch_jb = lch_jb;

  locals.ra = ((int (*)(void *, void *, int))locals.lch_fptr)(
      locals.lch_env, locals.lch_jb, locals.n);
  locals.rb = ((int (*)(void *, void *, int))locals.lch_fptr)(
      locals.lch_env, locals.lch_jb, ({ locals.ra - 1; }));
  locals.rc = ((int (*)(void *, void *, int))locals.lch_fptr)(
      locals.lch_env, locals.lch_jb, ({ locals.rb - 1; }));
  if (({ ({ ({ locals.ra + locals.rb; }) + locals.rc; }) == locals.s; })) {
    locals.env->cnt = ({ locals.env->cnt + 1; });
  } else {
    return 0;
  };
}

int fn2_body_wrapper(mp_prompt_t* p, void *env) {
  fn2_body_wrapper_locals_t locals;
  locals.env = (fn2_body_wrapper_env_t *)env;

  fn1(locals.env->countTriples_env, 500, 127, locals.env->countTriples_fptr,
      env, locals.env->countTriples_jb);

  return Print(locals.env->cnt);
}

int main() {
  main_locals_t locals;

  locals.cnt = 0;
  locals.countTriples_fptr = (void *)fn1;
  locals.countTriples_env = &locals;
  mp_prompt(fn2_body_wrapper, &locals);
}
