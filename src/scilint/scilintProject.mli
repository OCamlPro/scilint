
val add_to_path : (* directory *) string -> unit

type fun_decl = {
  fun_name : string;
  fun_args : string array;
  fun_loc : string * ScilabAst.location;
}
val declare_function : fun_decl -> unit

type fun_status =
    FunDeclared of fun_decl
  | FunFile of string
  | FunPrimitive
  | FunUnknown

val find_function : string -> fun_status


(* returns None, if there is no hint for the argument.
   returns Some list, if there is a list of possible string choices
      If "_" is first in the list, it means other types are possible
      If "_" is not the first in the list, a warning should be displayed
         if some other constant else than a string is given. *)
val find_argument : string -> int -> string list option
