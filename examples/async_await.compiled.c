
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
  int partialResult;
  void *runq;
  void *suspend_fptr;
  void *suspend_env;
  void *runNext_fptr;
  void *runNext_env;
  void *spawn_fptr;
  void *spawn_env;
  int nYield;
  int nJob;
  void *job_fptr;
  void *job_env;
  void *start_fptr;
  void *start_env;
} main_locals_t;

typedef main_locals_t suspend_1_env_t;
typedef struct suspend_1_locals_t {
  suspend_1_env_t *env;
  void *r;
} suspend_1_locals_t;

typedef main_locals_t runNext_2_env_t;
typedef struct runNext_2_locals_t {
  runNext_2_env_t *env;
  void *k;
} runNext_2_locals_t;

typedef main_locals_t spawnRec_env_t;
typedef struct spawnRec_locals_t {
  spawnRec_env_t *env;
  void *f_fptr;
  void *f_env;
  int spawnRec_env;
  void *lyield_fptr;
  void *lyield_env;
  jmp_buf *lyield_jb;
  void *lfork_fptr;
  void *lfork_env;
  jmp_buf *lfork_jb;
} spawnRec_locals_t;

typedef spawnRec_locals_t lyield_3_env_t;
typedef struct lyield_3_locals_t {
  lyield_3_env_t *env;
  void *jb;
  void *k;
} lyield_3_locals_t;

typedef spawnRec_locals_t lfork_4_env_t;
typedef struct lfork_4_locals_t {
  lfork_4_env_t *env;
  void *jb;
  void *forkf_fptr;
  void *forkf_env;
  void *k;
} lfork_4_locals_t;

typedef main_locals_t job_5_env_t;
typedef struct job_5_locals_t {
  job_5_env_t *env;
  void *lyield_fptr;
  void *lyield_env;
  void *lyield_jb;
  void *lfork_fptr;
  void *lfork_env;
  void *lfork_jb;
  void *job0_fptr;
  void *job0_env;
} job_5_locals_t;

typedef job_5_locals_t jobRec_env_t;
typedef struct jobRec_locals_t {
  jobRec_env_t *env;
  int i;
  void *lyield_fptr;
  void *lyield_env;
  void *lyield_jb;
  void *lfork_fptr;
  void *lfork_env;
  void *lfork_jb;
  int jobRec_env;
} jobRec_locals_t;

typedef main_locals_t start_6_env_t;
typedef struct start_6_locals_t {
  start_6_env_t *env;
  void *lyield_fptr;
  void *lyield_env;
  void *lyield_jb;
  void *lfork_fptr;
  void *lfork_env;
  void *lfork_jb;
  void *start0_fptr;
  void *start0_env;
} start_6_locals_t;

typedef start_6_locals_t startRec_env_t;
typedef struct startRec_locals_t {
  startRec_env_t *env;
  int i;
  void *lyield_fptr;
  void *lyield_env;
  void *lyield_jb;
  void *lfork_fptr;
  void *lfork_env;
  void *lfork_jb;
  int startRec_env;
} startRec_locals_t;
void suspend_1(suspend_1_env_t *env, void *r);
void runNext_2(runNext_2_env_t *env);
void lyield_3(lyield_3_env_t *env, void *jb, void *k);
void lfork_4(lfork_4_env_t *env, void *jb, void *forkf_fptr, void *forkf_env,
             void *k);
void spawnRec(spawnRec_env_t *env, void *f_fptr, void *f_env);
void jobRec(jobRec_env_t *env, int i, void *lyield_fptr, void *lyield_env,
            void *lyield_jb, void *lfork_fptr, void *lfork_env, void *lfork_jb);
void job_5(job_5_env_t *env, void *lyield_fptr, void *lyield_env,
           void *lyield_jb, void *lfork_fptr, void *lfork_env, void *lfork_jb);
void startRec(startRec_env_t *env, int i, void *lyield_fptr, void *lyield_env,
              void *lyield_jb, void *lfork_fptr, void *lfork_env,
              void *lfork_jb);
void start_6(start_6_env_t *env, void *lyield_fptr, void *lyield_env,
             void *lyield_jb, void *lfork_fptr, void *lfork_env,
             void *lfork_jb);
int main();

void suspend_1(suspend_1_env_t *env, void *r) {
  suspend_1_locals_t locals;
  locals.env = env;
  locals.r = r;

  return ListAppendCont(locals.env->runq, locals.r);
}

void runNext_2(runNext_2_env_t *env) {
  runNext_2_locals_t locals;
  locals.env = env;

  if (ListIsEmpty(locals.env->runq)) {
    return;
  } else {
    locals.k = ListPopFirstElementCont(locals.env->runq);
    mp_resume(locals.k, (void *)return;);
  }
}

void lyield_3(lyield_3_env_t *env, void *jb, void *k) {
  lyield_3_locals_t locals;
  locals.env = env;
  locals.jb = jb;
  locals.k = k;

  jmpret = suspend_1(locals.env->env->suspend_env, locals.k);
  runNext_2(locals.env->env->runNext_env);
  longjmp(locals.jb, 1);
}

void lfork_4(lfork_4_env_t *env, void *jb, void *forkf_fptr, void *forkf_env,
             void *k) {
  lfork_4_locals_t locals;
  locals.env = env;
  locals.jb = jb;
  locals.forkf_fptr = forkf_fptr;
  locals.forkf_env = forkf_env;
  locals.k = k;

  jmpret = suspend_1(locals.env->env->suspend_env, locals.k);
  spawnRec(locals.env->spawnRec_env, locals.forkf_fptr, locals.forkf_env);
  longjmp(locals.jb, 1);
}

void spawnRec(spawnRec_env_t *env, void *f_fptr, void *f_env) {
  spawnRec_locals_t locals;
  locals.env = env;
  locals.f_fptr = f_fptr;
  locals.f_env = f_env;

  locals.spawnRec_env = locals.env;
  locals.lyield_fptr = (void *)lyield_3;
  locals.lyield_env = &locals;
  locals.lyield_jb = ({
    jmp_buf tmp_buf;
    &tmp_buf;
  });
  if (setjmp(locals.lyield_jb)) {
    locals.lfork_fptr = (void *)lfork_4;
    locals.lfork_env = &locals;
    locals.lfork_jb = ({
      jmp_buf tmp_buf;
      &tmp_buf;
    });
    if (setjmp(locals.lfork_jb)) {
      return ((void (*)(void *, void *, void *, void *, void *, void *,
                        void *))locals.f_fptr)(
          locals.f_env, locals.lyield_fptr, locals.lyield_env, locals.lyield_jb,
          locals.lfork_fptr, locals.lfork_env, locals.lfork_jb);
    } else {
      return jmpret;
    };
  } else {
    return jmpret;
  };
}

void jobRec(jobRec_env_t *env, int i, void *lyield_fptr, void *lyield_env,
            void *lyield_jb, void *lfork_fptr, void *lfork_env,
            void *lfork_jb) {
  jobRec_locals_t locals;
  locals.env = env;
  locals.i = i;
  locals.lyield_fptr = lyield_fptr;
  locals.lyield_env = lyield_env;
  locals.lyield_jb = lyield_jb;
  locals.lfork_fptr = lfork_fptr;
  locals.lfork_env = lfork_env;
  locals.lfork_jb = lfork_jb;

  locals.jobRec_env = locals.env;
  if (({ 0 < locals.i; })) {

    ((void (*)(void *, void *))locals.lyield_fptr)(locals.lyield_env,
                                                   locals.lyield_jb);
    locals.env->env->partialResult = ({ locals.env->env->partialResult + 1; });
    __attribute__((musttail)) return jobRec(
        locals.jobRec_env, ({ locals.i - 1; }), locals.lyield_fptr,
        locals.lyield_env, locals.lyield_jb, locals.lfork_fptr,
        locals.lfork_env, locals.lfork_jb);
  } else {
    return;
  };
}

void job_5(job_5_env_t *env, void *lyield_fptr, void *lyield_env,
           void *lyield_jb, void *lfork_fptr, void *lfork_env, void *lfork_jb) {
  job_5_locals_t locals;
  locals.env = env;
  locals.lyield_fptr = lyield_fptr;
  locals.lyield_env = lyield_env;
  locals.lyield_jb = lyield_jb;
  locals.lfork_fptr = lfork_fptr;
  locals.lfork_env = lfork_env;
  locals.lfork_jb = lfork_jb;

  locals.job0_fptr = (void *)jobRec;
  locals.job0_env = &locals;
  return jobRec(locals.job0_env, locals.env->nYield, locals.lyield_fptr,
                locals.lyield_env, locals.lyield_jb, locals.lfork_fptr,
                locals.lfork_env, locals.lfork_jb);
}

void startRec(startRec_env_t *env, int i, void *lyield_fptr, void *lyield_env,
              void *lyield_jb, void *lfork_fptr, void *lfork_env,
              void *lfork_jb) {
  startRec_locals_t locals;
  locals.env = env;
  locals.i = i;
  locals.lyield_fptr = lyield_fptr;
  locals.lyield_env = lyield_env;
  locals.lyield_jb = lyield_jb;
  locals.lfork_fptr = lfork_fptr;
  locals.lfork_env = lfork_env;
  locals.lfork_jb = lfork_jb;

  locals.startRec_env = locals.env;
  if (({ 0 < locals.i; })) {

    ((void (*)(void *, void *, void *, void *))locals.lfork_fptr)(
        locals.lfork_env, locals.lfork_jb, locals.env->env->job_fptr,
        locals.env->env->job_env);
    __attribute__((musttail)) return startRec(
        locals.startRec_env, ({ locals.i - 1; }), locals.lyield_fptr,
        locals.lyield_env, locals.lyield_jb, locals.lfork_fptr,
        locals.lfork_env, locals.lfork_jb);
  } else {
    return;
  };
}

void start_6(start_6_env_t *env, void *lyield_fptr, void *lyield_env,
             void *lyield_jb, void *lfork_fptr, void *lfork_env,
             void *lfork_jb) {
  start_6_locals_t locals;
  locals.env = env;
  locals.lyield_fptr = lyield_fptr;
  locals.lyield_env = lyield_env;
  locals.lyield_jb = lyield_jb;
  locals.lfork_fptr = lfork_fptr;
  locals.lfork_env = lfork_env;
  locals.lfork_jb = lfork_jb;

  locals.start0_fptr = (void *)startRec;
  locals.start0_env = &locals;
  return startRec(locals.start0_env, locals.env->nJob, locals.lyield_fptr,
                  locals.lyield_env, locals.lyield_jb, locals.lfork_fptr,
                  locals.lfork_env, locals.lfork_jb);
}

int main() {
  main_locals_t locals;

  locals.partialResult = 0;
  locals.runq = ListNew();
  locals.suspend_fptr = (void *)suspend_1;
  locals.suspend_env = &locals;
  locals.runNext_fptr = (void *)runNext_2;
  locals.runNext_env = &locals;
  locals.spawn_fptr = (void *)spawnRec;
  locals.spawn_env = &locals;
  locals.nYield = 100;
  locals.nJob = 100100;
  locals.job_fptr = (void *)job_5;
  locals.job_env = &locals;
  locals.start_fptr = (void *)start_6;
  locals.start_env = &locals;

  spawnRec(locals.spawn_env, locals.start_fptr, locals.start_env);
  return Print(locals.partialResult);
}
