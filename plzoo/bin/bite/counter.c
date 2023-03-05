#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

struct closure_env_1_arg_1 {
    void **env;
    long (*f_ptr)(void*);
};

struct closure_env_1_arg_2 {
    void **env;
    long (*f_ptr)(void*, void*);
};

long put_handler(long n, void** env) {
    long *s = (long*)env[0];
    *s = n;
    return 0;
}

long get_handler(void** env) {
    long *s = (long*)env[0];
    return *s;
}

long counter(struct closure_env_1_arg_2* l_put, struct closure_env_1_arg_1* l_get, long n) {
    long i = l_get->f_ptr(l_get->env);
    if (i == 0) {
        return n; 
    } else {
        l_put->f_ptr((void*)i-1, l_put->env);
        return counter(l_put, l_get, n+1);
    }
}

const long init = 10;

int main() {
    void* locals[1] = {init};

    struct closure_env_1_arg_2 put_handler_closure = {&locals, &put_handler};

    struct closure_env_1_arg_1 get_handler_closure = {&locals, &get_handler};

    counter(&put_handler_closure, &get_handler_closure, 0);
}