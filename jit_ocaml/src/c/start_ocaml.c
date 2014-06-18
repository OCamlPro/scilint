int start_ocaml(){
  char *v[1];
  v[0] = "";
  caml_startup(v);
  return 0;
}
