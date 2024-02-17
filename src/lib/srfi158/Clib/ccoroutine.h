#ifndef BGL_COROUTINE
#define BGL_COROUTINE

#include <ucontext.h>
#include <signal.h>
#include "bigloo.h"

typedef struct bgl_coroutine {
  ucontext_t parent;
  ucontext_t self;
  obj_t thunk;
  obj_t ret_val;
  char* parent_stack_bottom;
  char* self_stack_bottom;
  char* stack[];
} *bgl_coroutine_t;


bgl_coroutine_t bgl_make_coroutine(obj_t thunk);

obj_t bgl_coroutine_call(bgl_coroutine_t cor);

obj_t bgl_coroutine_yield(bgl_coroutine_t cor, obj_t val);

void bgl_coroutine_finalize(bgl_coroutine_t cor, obj_t val);

#endif // BGL_COROUTINE
