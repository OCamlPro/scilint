type ctx
type var = private nativeint

(* Common *)
external get_var_address_from_name: ctx -> string -> var = "caml_get_var_address_from_name"
external get_var_address_from_position: ctx -> int -> var  = "caml_get_var_address_from_position"
external get_var_name_from_position: ctx -> int -> string  = "caml_get_var_name_from_position"
external get_nb_input_argument: ctx -> int = "caml_get_nb_input_argument"
external get_nb_output_argument: ctx -> int = "caml_get_nb_output_argument"
external return_arguments: ctx -> int = "caml_return_arguments"
external call_overload_function: ctx -> int -> string -> int -> int = "caml_call_overload_function"
external call_scilab_function: ctx -> string -> int -> int -> int -> int = "caml_call_scilab_function"
external get_var_type: ctx -> var -> int = "caml_get_var_type"
external get_named_var_type: ctx -> string -> int = "caml_get_named_var_type"
external get_var_dimension: ctx -> var -> int * int = "caml_get_var_dimension"
external get_named_var_dimension: ctx -> string -> int * int  ="caml_get_named_var_dimension"
external increase_val_ref: ctx -> var -> bool = "caml_increase_val_ref"
external decrease_val_ref: ctx -> var -> bool = "caml_decrease_val_ref"
external check_input_argument: ctx -> int -> int -> bool = "caml_check_input_argument"
external check_input_argument_at_least: ctx -> int -> bool = "caml_check_input_argument_at_least"
external check_input_argument_at_most: ctx -> int -> bool = "caml_check_input_argument_at_most"
external check_output_argument: ctx -> int -> int -> bool = "caml_check_output_argument"
external check_output_argument_at_least: ctx -> int -> bool = "caml_check_output_argument_at_least"
external check_output_argument_at_most: ctx -> int -> bool = "caml_check_output_argument_at_most"
external is_named_var_exist: ctx -> string -> bool = "caml_is_named_var_exist"
external delete_named_variable: ctx -> string -> bool = "caml_delete_named_variable"
external is_var_complex: ctx -> var -> bool = "caml_is_var_complex"
external is_named_var_complex: ctx -> string -> bool = "caml_is_named_var_complex"
external is_var_matrix_type: ctx -> var -> bool = "caml_is_var_matrix_type"
external is_named_var_matrix_type: ctx -> string -> bool = "caml_is_named_var_matrix_type"
external is_row_vector: ctx -> var -> bool = "caml_is_row_vector"
external is_named_row_vector: ctx -> string -> bool = "caml_is_named_row_vector"
external is_column_vector: ctx -> var -> bool = "caml_is_column_vector"
external is_named_column_vector: ctx -> string -> bool = "caml_is_named_column_vector"
external is_vector: ctx -> var -> bool = "caml_is_vector"
external is_named_vector: ctx -> string -> bool = "caml_is_named_vector"
external is_scalar: ctx -> var -> bool = "caml_is_scalar"
external is_named_scalar: ctx -> string -> bool = "caml_is_named_scalar"
external is_square_matrix: ctx -> var -> bool = "caml_is_square_matrix"
external is_named_square_matrix: ctx -> string -> bool = "caml_is_named_square_matrix"
external is_empty_matrix: ctx -> var -> bool = "caml_is_empty_matrix"
external is_named_empty_matrix: ctx -> string -> bool = "caml_is_named_empty_matrix"
external create_empty_matrix: ctx -> int -> bool = "caml_create_empty_matrix"
external create_named_empty_matrix: ctx -> string -> bool = "caml_create_named_empty_matrix"

(* String *)
external is_string_type: ctx -> var -> bool = "caml_is_string_type"
external is_named_string_type: ctx -> string -> bool = "caml_is_named_string_type"
external get_allocated_single_string: ctx -> var -> string = "caml_get_allocated_single_string"
external get_allocated_named_single_string: ctx -> var -> string = "caml_get_allocated_named_single_string"
external create_single_string: ctx -> int -> string -> int = "caml_create_single_string"
external create_named_single_string: ctx -> string -> string -> int = "caml_create_named_single_string"
external get_matrix_of_string: ctx -> var -> string array = "caml_get_matrix_of_string"
external get_named_matrix_of_string: ctx -> string -> string array = "caml_get_named_matrix_of_string"
external create_matrix_of_string: ctx -> int -> int -> int -> string array -> int = "caml_create_matrix_of_string"
external create_named_matrix_of_string: ctx -> string -> int -> int -> string array -> int = "caml_create_named_matrix_of_string"

(* Double *)
external is_double_type: ctx -> var -> bool = "caml_is_double_type"
external is_named_double_type: ctx -> string -> bool = "caml_is_named_double_type"
external get_scalar_double: ctx -> var -> float = "caml_get_scalar_double"
external get_named_scalar_double: ctx -> var -> float = "caml_get_named_scalar_double"
external get_scalar_complex_double: ctx -> var -> float * float = "caml_get_scalar_complex_double"
external get_named_scalar_complex_double: ctx -> string -> float * float = "caml_get_scalar_complex_double"
external create_scalar_double: ctx -> int -> float -> int = "caml_create_scalar_double"
external create_named_scalar_double: ctx -> string -> float -> int = "caml_create_named_scalar_double"
external create_scalar_complex_double: ctx -> int -> float -> int = "caml_create_scalar_complex_double"
external create_named_scalar_complex_double: ctx -> string -> float -> int = "caml_create_named_scalar_complex_double"
external get_matrix_of_double: ctx -> var -> float array = "caml_get_matrix_of_double"
external get_named_matrix_of_double: ctx -> string -> float array = "caml_get_named_matrix_of_double"
external get_complex_matrix_of_double: ctx -> var -> float array * float array = "caml_get_complex_matrix_of_double"
external get_named_complex_matrix_of_double: ctx -> string -> float array * float array = "caml_get_named_complex_matrix_of_double"
external create_matrix_of_double: ctx -> int -> int -> int -> float array -> int = "caml_create_matrix_of_double"
external create_named_matrix_of_double: ctx -> string -> int -> int -> float array -> int = "caml_create_named_matrix_of_double"
external create_complex_matrix_of_double: ctx -> int * int -> int -> float array -> float array -> int = "caml_create_complex_matrix_of_double"
external create_named_complex_matrix_of_double: ctx -> string -> int * int -> float array -> float array -> int = "caml_create_named_complex_matrix_of_double"

(* Boolean *)
(* external is_boolean_type: ctx -> var -> bool = "caml_is_boolean_type" *)
(* external is_named_boolean_type: ctx -> string -> bool = "caml_is_named_boolean_type" *)
(* external get_scalar_boolean: ctx -> var -> bool = "caml_get_scalar_boolean" *)
(* external get_named_scalar_boolean: ctx -> var -> bool = "caml_get_named_scalar_boolean" *)
(* external create_scalar_boolean: ctx -> int -> bool -> int = "caml_create_scalar_boolean" *)
(* external create_named_scalar_boolean: ctx -> string -> bool -> int = "caml_create_named_scalar_boolean" *)
(* external get_matrix_of_boolean: ctx -> var -> bool array = "caml_get_matrix_of_boolean" *)
(* external get_named_matrix_of_boolean: ctx -> string -> bool array = "caml_get_named_matrix_of_boolean" *)
(* external create_matrix_of_boolean: ctx -> int -> int -> int -> bool array -> int = "caml_create_matrix_of_boolean" *)
(* external create_named_matrix_of_boolean: ctx -> string -> int -> int -> bool array -> int = "caml_create_named_matrix_of_boolean" *)


let print_info_from_addr ctx addr =
  let typ = get_var_type ctx addr in
  Printf.printf "\n@%s --> type : %i%!" (Nativeint.to_string (addr:>nativeint)) typ;
  let (row, col) = get_var_dimension ctx addr in
  Printf.printf "(%i x %i)%!" row col;
  let is_str = is_string_type ctx addr in
  Printf.printf "; is_string : %s%!" (string_of_bool is_str);
  let is_complex = is_var_complex ctx addr in
  Printf.printf "; is_complex : %s%!" (string_of_bool is_complex);
  let is_matrix = is_var_matrix_type ctx addr in
  Printf.printf "; is_matrix : %s%!" (string_of_bool is_matrix)

let print_info_from_name ctx var_name =
  let typ = get_named_var_type ctx var_name in
  Printf.printf "\n%s --> type : %i%!" var_name typ;
  let (row, col) = get_named_var_dimension ctx var_name in
  Printf.printf "(%i x %i)%!" row col;
  let is_str = is_named_string_type ctx var_name in
  Printf.printf "; is_string : %s%!" (string_of_bool is_str);
  let is_complex = is_named_var_complex ctx var_name in
  Printf.printf "; is_complex : %s%!" (string_of_bool is_complex);
  let is_matrix = is_named_var_matrix_type ctx var_name in
  Printf.printf "; is_matrix : %s%!" (string_of_bool is_matrix)

let print_mat_string ctx var_name =
  let str_array = get_named_matrix_of_string ctx var_name in
  Printf.printf ";[%!";
  Array.iter (Printf.printf "%s;%!") str_array;
  Printf.printf "]\n%!"

let print_mat_double ctx var_name =
  let dbl_array = get_named_matrix_of_double ctx var_name in
  Printf.printf " = [%!";
  Array.iter (Printf.printf "%f;%!") dbl_array;
  Printf.printf "]\n%!"

let print_mat_complex ctx var_name =
  let (real_array, complex_array) = 
    get_named_complex_matrix_of_double ctx var_name in
  Printf.printf " = [%!";
  Array.iteri (fun i r -> 
      Printf.printf "%f + %fi;%!" r complex_array.(i)) real_array;
  Printf.printf "]\n%!"

let test ctx = 
  (* Test on common *)
  Printf.printf "Starting test..\n%!";
  let b = create_named_empty_matrix ctx "empt" in
  let addr = get_var_address_from_name ctx "empt" in
  print_info_from_addr ctx addr;
  print_mat_double ctx "empt";
  let arr1 = Array.make 4 "cc" in
  let _ = create_named_matrix_of_string ctx "mat_string" 2 2 arr1 in
  print_info_from_name ctx "mat_string";
  print_mat_string ctx "mat_string";
  let arr2 = Array.make 2 5.0 in
  let _ = create_named_matrix_of_double ctx "mat_double" 2 1 arr2 in
  print_info_from_name ctx "mat_double";
  print_mat_double ctx "mat_double";
  let arr3 = Array.make 4 5.0 in
  let arr4 = Array.make 4 2.0 in
  let _ = 
    create_named_complex_matrix_of_double ctx "mat_complex" (2,2) arr3 arr4 in
  print_info_from_name ctx "mat_complex";
  print_mat_complex ctx "mat_complex"
    
