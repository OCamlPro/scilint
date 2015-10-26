#include <stdlib.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>

static int ocaml_started = 0;

void init_caml_runtime()
{
  char *v[1];
  v[0] = "";
  caml_startup(v);
  ocaml_started = 1;
}

int call_caml_function(int function_id)
{
  int i;
  static value * closure_v = NULL;
  if (closure_v == NULL) closure_v = caml_named_value("jit");
  // Create CAMLvalue for the arguments
  caml_callback(*closure_v, Val_int(function_id));
  CAMLreturn(1);
}

int cjit_call_function(int function_id)
{
  if (!ocaml_started) init_caml_runtime();
  return call_caml_function(ctx, fn);
}

int call_caml_load_filename(void* ctx, char * filename)
{
  int i;
  static value * closure_v = NULL;
  if (closure_v == NULL) closure_v = caml_named_value("jit_load");
  // Create CAMLvalue for the arguments
  CAMLlocal1( filename_v );
  filename_v = caml_copy_string(filename);
  caml_callback2(*closure_v, ctx, filename_v);
  return 0;
}

int cjit_load(void* ctx, char * fn)
{
  if (!ocaml_started) init_caml_runtime();
  return call_caml_load_filename(ctx, fn);
}
