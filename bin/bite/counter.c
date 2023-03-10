#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

struct get_closure {
    struct main_env *env;
    long (*f_ptr)(struct main_env *);
};

struct put_closure {
    struct main_env *env;
    long (*f_ptr)(long, struct main_env *);
};

struct main_env {
    long init;
};


long put_handler(long n, struct main_env* env) {
    long *s = &env->init;
    *s = n;
    return 0;
}

long get_handler(struct main_env* env) {
    return env->init;
}

long counter(struct put_closure* l_put, struct get_closure* l_get, long n) {
    long i = l_get->f_ptr(l_get->env);
    if (i == 0) {
        return n; 
    } else {
        l_put->f_ptr(i-1, l_put->env);
        return counter(l_put, l_get, n+1);
    }
}

const long init = 10;

int main() {

    struct main_env locals = {init};

    struct put_closure p = {&locals, &put_handler};

    struct get_closure g = {&locals, &get_handler};

    counter(&p, &g, 0);
}