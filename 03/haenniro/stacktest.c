#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void print(int64_t i) {
    printf("here's a print\n");
    char* buf = (char*)calloc(20, sizeof(char));
    snprintf((char *)buf, 20, "%ld", (long)i);
    free(buf);
}