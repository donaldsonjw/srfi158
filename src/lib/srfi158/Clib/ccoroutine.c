#include "ccoroutine.h"


static void* split_address_to_voidptr(int msb, int lsb) {
  uintptr_t res = (uintptr_t)msb;
  res = (res << 32);
  res = res | (0XFFFFFFFF & (uintptr_t)lsb);
  return (void*)res;
}

static void coroutine_start(int thunk_msb, int thunk_lsb,
                            int cor_msb, int cor_lsb) {
  obj_t thunk = (obj_t) split_address_to_voidptr(thunk_msb, thunk_lsb);
  bgl_coroutine_t cor = (bgl_coroutine_t) split_address_to_voidptr(cor_msb, cor_lsb);
  obj_t env = BGL_CURRENT_DYNAMIC_ENV();

  // save parent stack bottom and establish self stack bottom
  cor->parent_stack_bottom =  BGL_DYNAMIC_ENV(env).stack_bottom;
  cor->self_stack_bottom = (char*)&thunk_msb;
  BGL_DYNAMIC_ENV(env).stack_bottom = cor->self_stack_bottom;
  
  apply(thunk, BNIL);
}

BGL_RUNTIME_DEF
bgl_coroutine_t bgl_make_coroutine(obj_t thunk) {
  bgl_coroutine_t cor = (bgl_coroutine_t)GC_MALLOC(sizeof(*cor) + SIGSTKSZ);
  cor->thunk=thunk;
  getcontext(&cor->self);
  cor->self.uc_link = &cor->parent;
  cor->self.uc_stack.ss_sp = cor->stack;
  cor->self.uc_stack.ss_size = SIGSTKSZ;

  makecontext(&cor->self, (void (*)(void))coroutine_start,
              4,
              (int)(((uintptr_t) thunk) >> 32),
              (int)(((uintptr_t) thunk) & 0xFFFFFFFF),
              (int)(((uintptr_t) cor) >> 32),
              (int)(((uintptr_t) cor) & 0xFFFFFFFF));
  return cor;
}

BGL_RUNTIME_DEF
obj_t bgl_coroutine_call(bgl_coroutine_t cor) {
  obj_t result;
  obj_t env = BGL_CURRENT_DYNAMIC_ENV();
  if (cor->thunk != BFALSE) {
    BGL_DYNAMIC_ENV(env).stack_bottom = cor->self_stack_bottom;
    swapcontext(&cor->parent, &cor->self);
    result = cor->ret_val;
    cor->ret_val = BUNSPEC;
  } else {
    result= cor->ret_val;
  }
  return result;
}

BGL_RUNTIME_DEF
obj_t bgl_coroutine_yield(bgl_coroutine_t cor, obj_t val) {
  obj_t env = BGL_CURRENT_DYNAMIC_ENV();
  cor->ret_val = val;
  BGL_DYNAMIC_ENV(env).stack_bottom = cor->parent_stack_bottom;
  swapcontext(&cor->self, &cor->parent);
  return BUNSPEC;
}

BGL_RUNTIME_DEF
void bgl_coroutine_finalize(bgl_coroutine_t cor, obj_t val) {
  obj_t env = BGL_CURRENT_DYNAMIC_ENV();
  cor->ret_val = val;
  cor->thunk = BFALSE;
  BGL_DYNAMIC_ENV(env).stack_bottom = cor->parent_stack_bottom;
 }
