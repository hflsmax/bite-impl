
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
  jmp_buf fn2_handler_wrapper_jb;
  int cnt;
  void *countTriples_fptr;
  void *countTriples_env;
  void *countTriples_jb;
  void *lch_fptr;
  void *lch_env;
  void *lch_jb;
} main_locals_t;

typedef main_locals_t fn1_env_t;
typedef struct fn1_locals_t {
  fn1_env_t *env;
  int n;
  int s;
  void *lch_fptr;
  void *lch_env;
  void *lch_jb;
  int ra;
  int rb;
  int rc;
} fn1_locals_t;

typedef main_locals_t fn2_handler_wrapper_env_t;
typedef struct fn2_handler_wrapper_locals_t {
  fn2_handler_wrapper_env_t *env;
  void *jb;
  int n;
  void *fn2_fptr;
  void *fn2_env;
  void *fn2_jb;
} fn2_handler_wrapper_locals_t;

typedef fn2_handler_wrapper_locals_t fn2_env_t;
typedef struct fn2_locals_t {
  fn2_env_t *env;
  void *jb;
  void *r;
  void *iter_fptr;
  void *iter_env;
  void *iter_jb;
} fn2_locals_t;

typedef fn2_locals_t iterRec_env_t;
typedef struct iterRec_locals_t {
  iterRec_env_t *env;
  int i;
} iterRec_locals_t;

typedef main_locals_t fn2_body_wrapper_env_t;
typedef struct fn2_body_wrapper_locals_t {
  fn2_body_wrapper_env_t *env;
} fn2_body_wrapper_locals_t;

int fn1(fn1_env_t *env, int n, int s, void *lch_fptr, void *lch_env,
        void *lch_jb) {
  fn1_locals_t locals;
  locals.env = env;
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

int iterRec(iterRec_env_t *env, int i) {
  iterRec_locals_t locals;
  locals.env = env;
  locals.i = i;

  if (({ locals.env->env->n < locals.i; })) {
    return 0;
  } else {

    mp_resume(locals.env->r, (void *)locals.i);
    __attribute__((musttail)) return iterRec(locals.env, ({ locals.i + 1; }));
  }
}

int fn2(fn2_env_t *env, void *jb, void *r) {
  fn2_locals_t locals;
  locals.env = env;
  locals.jb = jb;
  locals.r = r;

  locals.iter_fptr = (void *)iterRec;
  locals.iter_env = &locals;
  return iterRec(locals.iter_env, 1);
}

int fn2_handler_wrapper(fn2_handler_wrapper_env_t *env, void *jb, int n) {
  fn2_handler_wrapper_locals_t locals;
  locals.env = env;
  locals.jb = jb;
  locals.n = n;

  locals.fn2_fptr = (void *)fn2;
  locals.fn2_env = &locals;
  return fn2(locals.fn2_env);
}

int fn2_body_wrapper(mp_prompt_t *p, void *env) {
  fn2_body_wrapper_locals_t locals;
  locals.env = (fn2_handler_wrapper_env_t *)env;
  locals.env->lch_jb = p;
  return fn1(locals.env->countTriples_env, 500, 127, locals.env->lch_fptr,
             locals.env->lch_env, locals.env->lch_jb);
}

int main() {
  main_locals_t locals;

  locals.cnt = 0;
  locals.countTriples_fptr = (void *)fn1;
  locals.countTriples_env = &locals;

  ({
    locals.lch_fptr = (void *)fn2_handler_wrapper;
    locals.lch_env = &locals;
    mp_prompt(fn2_body_wrapper, &locals);
  });
  return Print(locals.cnt);
}
