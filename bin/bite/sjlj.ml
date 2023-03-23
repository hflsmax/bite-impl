let sjlj_def = function 
  | Config.GLIBC -> "#include <setjmp.h>"
  | Config.X64 -> {|
typedef struct __jmp_buf_tag {
    unsigned long __jb[2];
} jmp_buf[1];
int setjmp(jmp_buf) __attribute__((__returns_twice__));
__attribute__((__noreturn__)) void longjmp(jmp_buf, int);

__asm__(
".global _setjmp\n\t"
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
|}
  | Config.A64 -> {|
typedef struct __jmp_buf_tag {
    unsigned long __jb[2];
} jmp_buf[1];
int setjmp(jmp_buf) __attribute__((__returns_twice__));
__attribute__((__noreturn__)) void longjmp(jmp_buf, int);

__asm__(
".global __setjmp\n\t"
".global _setjmp\n\t"
".global setjmp\n\t"
".type __setjmp,@function\n\t"
".type _setjmp,@function\n\t"
".type setjmp,@function\n\t"
"__setjmp:\n\t"
"_setjmp:\n\t"
"setjmp:\n\t"
"mov x2, sp\n\t"
"stp x2, x30, [x0,#0]\n\t"
"mov x0, #0\n\t"
"ret\n\t"

".global _longjmp\n\t"
".global longjmp\n\t"
".type _longjmp,%function\n\t"
".type longjmp,%function\n\t"
"_longjmp:\n\t"
"longjmp:\n\t"
"ldp x2, x30, [x0,#0]\n\t"
"mov sp, x2\n\t"

"cmp w1, 0\n\t"
"csinc w0, w1, wzr, ne\n\t"
"br x30\n\t");
|}