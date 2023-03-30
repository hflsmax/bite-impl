#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

#include <mprompt.h>
struct main {
    int* cnt;
    int n;
    int s;
};


void* choose(mp_resume_t* r, void* n) {
    mp_resume_t* multi_r = mp_resume_multi(r);
    for (int i = 1; i <= (int)n; i++) {
        if (i != n) {
            mp_resume_dup(multi_r);
            mp_resume(multi_r, (void*)i);
        } else {
            mp_resume_dup(multi_r);
            mp_resume(multi_r, (void*)i);
        }
    }
}

void* count_triples(mp_prompt_t* p, void* _main) {
    struct main *main = (struct main*)_main;
    int r1 = (int)mp_yield(p, choose, (void*)main->n);
    int r2 = (int)mp_yield(p, choose, (void*)r1-1);
    int r3 = (int)mp_yield(p, choose, (void*)r2-1);
    if (r1 + r2 + r3 == main->s)
        *(main->cnt) += 1;
}


int main() {
    int cnt = 0;
    mp_prompt(count_triples, &(struct main){&cnt, 500, 127});
    printf("cnt = %d\n", cnt);
}