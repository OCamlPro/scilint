#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>

int call_caml(void* ctx){
  int i;
  static value * f_closure = NULL;
  if (f_closure == NULL) f_closure = caml_named_value("jit_test");
  // Create CAMLvalue for the arguments
  CAMLparam1( ctx );
  caml_callback(*f_closure, ctx);
  return 0;
}

int cjit_test(void* ctx){
  static int ocaml_started = 0;
  if (ocaml_started) return call_caml(ctx);
  else {
    char *v[1];
    v[0] = "";
    caml_startup(v);
    ocaml_started = 1;
    return call_caml(ctx);
  }
}
