#include <stdint.h>

int64_t ll_callback(int64_t (*fun)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)) {
  return fun(1,2,3,4,5,6,7,8,9,10);
}