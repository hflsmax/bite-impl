
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
  int arrLen;
  void *arr;
  int iterIdx;
  void *iterNext_fptr;
  void *iterNext_env;
  void *iterNext_jb;
  void *add_fptr;
  void *add_env;
  void *add_jb;
  void *foldLeft_fptr;
  void *foldLeft_env;
  void *foldLeft_jb;
} main_locals_t;

typedef main_locals_t fn1_env_t;
typedef struct fn1_locals_t {
  fn1_env_t *env;
  void *exc_fptr;
  void *exc_env;
  void *exc_jb;
} fn1_locals_t;

typedef main_locals_t fn2_env_t;
typedef struct fn2_locals_t {
  fn2_env_t *env;
  int a;
  int b;
} fn2_locals_t;

typedef main_locals_t foldLeftRec_env_t;
typedef struct foldLeftRec_locals_t {
  jmp_buf fn3_jb;
  foldLeftRec_env_t *env;
  void *op_fptr;
  void *op_env;
  int acc;
  int next;
  bool toBreak;
  void *exc_fptr;
  void *exc_env;
  void *exc_jb;
} foldLeftRec_locals_t;

typedef foldLeftRec_locals_t fn3_env_t;
typedef struct fn3_locals_t {
  fn3_env_t *env;
  void *jb;
} fn3_locals_t;
bool fn3_saved = false;
jmp_buf fn3_jb;

int fn1(fn1_env_t *env, void *exc_fptr, void *exc_env, void *exc_jb) {
  fn1_locals_t locals;
  locals.env = env;
  locals.exc_fptr = exc_fptr;
  locals.exc_env = exc_env;
  locals.exc_jb = exc_jb;

  if (({ locals.env->iterIdx < locals.env->arrLen; })) {

    locals.env->iterIdx = ({ locals.env->iterIdx + 1; });
    return ArrayGet(locals.env->arr, ({ locals.env->iterIdx - 1; }));
  } else {
    return ((int (*)(void *, void *))locals.exc_fptr)(locals.exc_env,
                                                      locals.exc_jb);
  }
}

int fn2(fn2_env_t *env, int a, int b) {
  fn2_locals_t locals;
  locals.env = env;
  locals.a = a;
  locals.b = b;

  return ({ locals.a + locals.b; });
}

int fn3(fn3_env_t *env, void *jb) {
  fn3_locals_t locals;
  locals.env = env;
  locals.jb = jb;

  locals.env->toBreak = true;
  jmpret = 0;
  _longjmp(jb, 1);
}

int foldLeftRec(foldLeftRec_env_t *env, void *op_fptr, void *op_env, int acc) {
  foldLeftRec_locals_t locals;
  locals.env = env;
  locals.op_fptr = op_fptr;
  locals.op_env = op_env;
  locals.acc = acc;

  locals.next = 0;
  locals.toBreak = false;

  ({
    locals.exc_fptr = (void *)fn3;
    locals.exc_env = &locals;
    locals.exc_jb = &fn3_jb;

    (fn3_saved || _setjmp(locals.exc_jb) == 0 ? ({
      fn3_saved = true;
      locals.next = fn1(locals.env->iterNext_env, locals.exc_fptr,
                        locals.exc_env, locals.exc_jb);
    })
                                              : ({ jmpret; }));
  });
  if (locals.toBreak) {
    return locals.acc;
  } else {
    __attribute__((musttail)) return foldLeftRec(
        locals.env, locals.op_fptr, locals.op_env,
        ((int (*)(void *, int, int))locals.op_fptr)(locals.op_env, locals.acc,
                                                    locals.next));
  };
}

int main() {
  main_locals_t locals;

  locals.arrLen = 100100100;
  locals.arr = ArrayInit(locals.arrLen);
  locals.iterIdx = 0;
  locals.iterNext_fptr = (void *)fn1;
  locals.iterNext_env = &locals;
  locals.add_fptr = (void *)fn2;
  locals.foldLeft_fptr = (void *)foldLeftRec;
  locals.foldLeft_env = &locals;
  return Print(
      foldLeftRec(locals.foldLeft_env, locals.add_fptr, locals.add_env, 0));
}
