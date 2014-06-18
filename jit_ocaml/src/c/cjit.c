#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>

int call_caml(char * expr, char ** vars, int* types, int* complexs, int** dims, int length){
  int i;
  static value * f_closure = NULL;
  if (f_closure == NULL) f_closure = caml_named_value("jit_expr");
  // Create CAMLvalue for the arguments
  CAMLparam0();
  CAMLlocal5( ml_expr, ml_vars, ml_types , ml_complex , ml_dims );
  ml_types = caml_alloc(length, 0);
  for (i = 0 ; i < length; i++) {
    Store_field (ml_types, i, Val_int(types[i]));
  }
  ml_complex = caml_alloc(length, 0);
  for (i = 0 ; i < length; i++) {
    Store_field (ml_complex, i, Val_int(complexs[i]));
  }
  ml_dims = caml_alloc(length * 2, 0);
  for (i = 0 ; i < length * 2; i = i + 2) {
    Store_field (ml_dims, i, Val_int(dims[i]));
    Store_field (ml_dims, i + 1, Val_int(dims[i + 1]));
  }
  ml_vars = caml_copy_string_array( (const char **) vars );
  ml_expr = caml_copy_string(expr);
  value args[5] = {  ml_expr, ml_vars, ml_types, ml_complex, ml_dims };
  caml_callbackN(*f_closure, 5, args);
  CAMLreturn(0);
}

int cjit(char * expr, char **vars, int * types, int* complexs, int**dims, int length){
  static int ocaml_started = 0;
  if (ocaml_started) return call_caml(expr, vars, types, complexs, dims, length);
  else {
    char *v[1];
    v[0] = "";
    caml_startup(v);
    ocaml_started = 1;
    return call_caml(expr, vars, types, complexs, dims, length);
  }
}
