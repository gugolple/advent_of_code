#define _GNU_SOURCE

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

#include<limits.h>
#include<stdio.h>
#include<stdlib.h>
#include<glib.h>
#include<assert.h>

#define STR_MAX 1024
#define NUM_DAYS 80
#define MAX_PHASE 9
int main(int argc, char** argv) {
    unsigned long fish[MAX_PHASE] = {0};
    char str[STR_MAX] = "\0";
    fgets(str, STR_MAX, stdin);
    while (!feof(stdin)) {
        int start = 0;
        char v;
        for(int i=0; str[i] != '\0'; i++) {
            if (str[i] == ',') {
                str[i] = '\0';
                v = atoi(&str[start]);
                fish[v]++;
                start = i+1;
            }
        }
        v = atoi(&str[start]);
        fish[v]++;
        fgets(str, STR_MAX, stdin);
    }

    printf("bucket status\n");
    for(int i=0; i<MAX_PHASE; i++) {
        printf("phase: %d count: %lu\n", i, fish[i]);
    }

    unsigned long total = 0;
    for(int i=0; i<NUM_DAYS; i++) {
        unsigned long next_iter = fish[0];
        for(int i=1; i<MAX_PHASE; i++) {
            fish[i-1] = fish[i];
        }
        fish[6] += next_iter;
        fish[8] = next_iter;

        total = 0;
        printf("bucket status\n");
        for(int i=0; i<MAX_PHASE; i++) {
            printf("phase: %d count: %lu\n", i, fish[i]);
            total += fish[i];
        }
        printf("Total: %lu\n", total);
    }

    printf("%lu\n", total);
    return 0;
}
