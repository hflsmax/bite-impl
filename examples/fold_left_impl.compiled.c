
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
  void *iterNext_fptr;
  void *add_fptr;
  void *foldLeft_fptr;
} main_locals_t;

typedef main_locals_t iterNext_1_env_t;
typedef struct iterNext_1_locals_t {
  iterNext_1_env_t *env;
  void *exc_fptr;
  void *exc_env;
  void *exc_jb;
} iterNext_1_locals_t;

typedef main_locals_t add_2_env_t;
typedef struct add_2_locals_t {
  add_2_env_t *env;
  int a;
  int b;
} add_2_locals_t;

typedef main_locals_t foldLeftRec_env_t;
typedef struct foldLeftRec_locals_t {
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

typedef foldLeftRec_locals_t exc_3_env_t;
typedef struct exc_3_locals_t {
  exc_3_env_t *env;
  void *jb;
} exc_3_locals_t;
int iterNext_1(iterNext_1_env_t *env, void *exc_fptr, void *exc_env,
               void *exc_jb);
int add_2(add_2_env_t *env, int a, int b);
int exc_3(exc_3_env_t *env, void *jb);
int foldLeftRec(foldLeftRec_env_t *env, void *op_fptr, void *op_env, int acc);
int main();
int arrLen;
void *arr;
int iterIdx = 0;
void *iterNext_env;
void *add_env;
void *foldLeft_env;

int iterNext_1(iterNext_1_env_t *env, void *exc_fptr, void *exc_env,
               void *exc_jb) {
  iterNext_1_locals_t locals;
  locals.env = env;
  locals.exc_fptr = exc_fptr;
  locals.exc_env = exc_env;
  locals.exc_jb = exc_jb;

  if (({ iterIdx < arrLen; })) {

    iterIdx = ({ iterIdx + 1; });
    return ArrayGet(arr, ({ iterIdx - 1; }));
  } else {
    return ((int (*)(void *, void *))locals.exc_fptr)(locals.exc_env,
                                                      locals.exc_jb);
  }
}

int add_2(add_2_env_t *env, int a, int b) {
  add_2_locals_t locals;
  locals.env = env;
  locals.a = a;
  locals.b = b;

  return ({ locals.a + locals.b; });
}

int exc_3(exc_3_env_t *env, void *jb) {
  exc_3_locals_t locals;
  locals.env = env;
  locals.jb = jb;

  jmpret = locals.env->toBreak = true;
  0;
  longjmp(locals.jb, 1);
}

int foldLeftRec(foldLeftRec_env_t *env, void *op_fptr, void *op_env, int acc) {
  foldLeftRec_locals_t locals;
  locals.env = env;
  locals.op_fptr = op_fptr;
  locals.op_env = op_env;
  locals.acc = acc;

  locals.next = 0;
  locals.toBreak = false;

  locals.exc_fptr = (void *)exc_3;
  locals.exc_env = &locals;
  locals.exc_jb = ({
    jmp_buf tmp_buf;
    &tmp_buf;
  });
  (!setjmp(locals.exc_jb) ? ({
    locals.next = iterNext_1(iterNext_env, locals.exc_fptr, locals.exc_env,
                             locals.exc_jb);
  })
                          : ({ jmpret; }));
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

  arrLen = 100100100;
  arr = ArrayInit(arrLen);
  locals.iterNext_fptr = (void *)iterNext_1;
  iterNext_env = &locals;
  locals.add_fptr = (void *)add_2;
  locals.foldLeft_fptr = (void *)foldLeftRec;
  return Print(foldLeftRec(foldLeft_env, locals.add_fptr, add_env, 0));
}
