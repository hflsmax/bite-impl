#include <stdio.h>


typedef struct closture_t {
    void *f_ptr;
    void *env;
} closure_t;
typedef struct main_env_t {} main_env_t;

typedef struct main_locals_t {
main_env_t* env;
closure_t* main;
closure_t* counter;
closure_t f;
closure_t* run;
closure_t g;
} main_locals_t;

typedef main_locals_t f_env_t;
typedef struct f_locals_t {
f_env_t* env;
closure_t* f;
int n;
closure_t* lget;
closure_t* lset;
int i;
} f_locals_t;

typedef main_locals_t g_env_t;
typedef struct g_locals_t {
g_env_t* env;
closure_t* g;
int n;
int s;
closure_t* lget;
closure_t fget;
closure_t* lset;
closure_t fset;
} g_locals_t;

typedef g_locals_t fget_env_t;
typedef struct fget_locals_t {
fget_env_t* env;
closure_t* fget;
} fget_locals_t;

typedef g_locals_t fset_env_t;
typedef struct fset_locals_t {
fset_env_t* env;
closure_t* fset;
int n;
} fset_locals_t;


int f(closure_t* closure, int n, closure_t* lget, closure_t* lset)
{
f_locals_t locals;
locals.f = closure;
locals.env = closure->env;
locals.n = n;
locals.lget = lget;
locals.lset = lset;
return ({
locals.i = ((int(*)(void*))locals.lget->f_ptr)(locals.lget);
(({locals.i == 0;}) ? ({locals.n;}) : ({
((int(*)(void*, int))locals.lset->f_ptr)(locals.lset, ({locals.i - 1;}));
((int(*)(void*, int, closure_t*, closure_t*))locals.f->f_ptr)(locals.f, ({locals.n + 1;}), locals.lget, locals.lset);}));});}


int fget(closure_t* closure)
{
fget_locals_t locals;
locals.fget = closure;
locals.env = closure->env;
return ({
locals.env->s
;});}


int fset(closure_t* closure, int n)
{
fset_locals_t locals;
locals.fset = closure;
locals.env = closure->env;
locals.n = n;
return ({
locals.env->s = locals.n;});}


int g(closure_t* closure, int n)
{
g_locals_t locals;
locals.g = closure;
locals.env = closure->env;
locals.n = n;
return ({
locals.s = locals.n;
locals.lget = ({locals.fget.f_ptr = fget;
locals.fget.env = &locals;
&locals.fget;});
locals.lset = ({locals.fset.f_ptr = fset;
locals.fset.env = &locals;
&locals.fset;});
((int(*)(void*, int, closure_t*, closure_t*))locals.env->counter->f_ptr)(locals.env->counter, 0, locals.lget, locals.lset);});}

int main() {
    main_locals_t locals;
    locals.f.f_ptr = (void*)f;
    locals.f.env = & locals;

    locals.g.f_ptr = (void*)g;
    locals.g.env = & locals;

    locals.counter = &locals.f;
    locals.run = &locals.g;

    int n = 100;
    // scanf("%d", &n);
    int out = ((int( * )(void * , int)) locals.run -> f_ptr)(locals.run, n);
    // printf("%d\n", out);
}