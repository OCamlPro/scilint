#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>

static int ocaml_started = 0;

int call_caml(double nbr){
  int i;
  static value * f_closure = NULL;
  if (f_closure == NULL) f_closure = caml_named_value("jit");
  // Create CAMLvalue for the arguments
  CAMLparam1( nbr );
  CAMLlocal1( ml_nbr );
  ml_nbr = caml_copy_double(nbr);
  caml_callback(*f_closure, ml_nbr);
  CAMLreturn(1);
}

int cjit(double nbr){
  if (ocaml_started) return call_caml(nbr);
  else {
    char *v[1];
    v[0] = "";
    caml_startup(v);
    ocaml_started = 1;
    return call_caml(nbr);
  }
}

/* int call_caml(char * expr, char ** vars, int* types, int* complexs, int** dims, int length){ */
/*   int i; */
/*   static value * f_closure = NULL; */
/*   if (f_closure == NULL) f_closure = caml_named_value("jit_expr"); */
/*   // Create CAMLvalue for the arguments */
/*   CAMLparam0(); */
/*   CAMLlocal5( ml_expr, ml_vars, ml_types , ml_complex , ml_dims ); */
/*   ml_types = caml_alloc(length, 0); */
/*   for (i = 0 ; i < length; i++) { */
/*     Store_field (ml_types, i, Val_int(types[i])); */
/*   } */
/*   ml_complex = caml_alloc(length, 0); */
/*   for (i = 0 ; i < length; i++) { */
/*     Store_field (ml_complex, i, Val_int(complexs[i])); */
/*   } */
/*   ml_dims = caml_alloc(length * 2, 0); */
/*   for (i = 0 ; i < length * 2; i = i + 2) { */
/*     Store_field (ml_dims, i, Val_int(dims[i])); */
/*     Store_field (ml_dims, i + 1, Val_int(dims[i + 1])); */
/*   } */
/*   ml_vars = caml_copy_string_array( (const char **) vars ); */
/*   ml_expr = caml_copy_string(expr); */
/*   value args[5] = {  ml_expr, ml_vars, ml_types, ml_complex, ml_dims }; */
/*   caml_callbackN(*f_closure, 5, args); */
/*   CAMLreturn(0); */
/* } */

/* int cjit(char * expr, char **vars, int * types, int* complexs, int**dims, int length){ */
/*   if (ocaml_started) return call_caml(expr, vars, types, complexs, dims, length); */
/*   else { */
/*     char *v[1]; */
/*     v[0] = ""; */
/*     caml_startup(v); */
/*     ocaml_started = 1; */
/*     return call_caml(expr, vars, types, complexs, dims, length); */
/*   } */
/* } */

int call_caml_load(void* ctx, char * fn){
  int i;
  static value * f_closure = NULL;
  if (f_closure == NULL) f_closure = caml_named_value("jit_load");
  // Create CAMLvalue for the arguments
  CAMLparam1( ctx );
  CAMLlocal1( ml_fn );
  ml_fn = caml_copy_string(fn);
  caml_callback2(*f_closure, ctx, ml_fn);
  return 0;
}

int cjit_load(void* ctx, char * fn){
  if (ocaml_started) return call_caml_load(ctx, fn);
  else {
    char *v[1];
    v[0] = "";
    caml_startup(v);
    ocaml_started = 1;
    return call_caml_load(ctx, fn);
  }
}


/* int call_caml_test(void* ctx){ */
/*   int i; */
/*   static value * f_closure = NULL; */
/*   if (f_closure == NULL) f_closure = caml_named_value("jit_test"); */
/*   // Create CAMLvalue for the arguments */
/*   CAMLparam1( ctx ); */
/*   caml_callback(*f_closure, ctx); */
/*   return 0; */
/* } */

/* int cjit_test(void* ctx){ */
/*   static int ocaml_started = 0; */
/*   if (ocaml_started) return call_caml_test(ctx); */
/*   else { */
/*     char *v[1]; */
/*     v[0] = ""; */
/*     caml_startup(v); */
/*     ocaml_started = 1; */
/*     return call_caml_test(ctx); */
/*   } */
/* } */
