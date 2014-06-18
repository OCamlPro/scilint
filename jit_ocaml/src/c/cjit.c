#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>

int call_caml(char * expr, char ** vars, int* types, int length){
  int i;
  static value * f_closure = NULL;
  if (f_closure == NULL) f_closure = caml_named_value("jit_expr");
  // Create CAMLvalue for the arguments
  CAMLparam0();
  CAMLlocal3( ml_expr, ml_vars, ml_types );
  ml_types = caml_alloc(length, 0);
  for (i = 0 ; i < length; i++) {
    Store_field (ml_types, i, Val_int(types[i]));
  }
  ml_vars = caml_copy_string_array( (const char **) vars );
  ml_expr = caml_copy_string(expr);
  caml_callback3(*f_closure, ml_expr, ml_vars, ml_types);
  CAMLreturn(0);
}

int cjit(char * expr, char **vars, int * types, int length){
  return call_caml(expr, vars, types, length);
}
