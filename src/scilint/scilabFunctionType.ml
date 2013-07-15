open ScilabTypedContext


let get_unique = function 
  | Simple (Type_arrow (args, rets)) -> (args,rets)
  | _ -> failwith "Constraint type for scilab functions should not contains Depend case"
      
let get_ret list_arg list_fun =
  List.map (fun vers -> match vers with
    | Simple (Type_arrow (args, rets)) -> 
        Depend (List.map (fun (i,sy) -> (sy.symbol_name, args.(i))) list_arg, (Array.get rets 0))
    | _ -> failwith "Constraint type for scilab functions should not contains Depend case") list_fun

let get_nth_arg nth l =
  List.map (fun vers -> match vers with
    | Simple (Type_arrow (args, _)) -> Simple (Array.get args nth)
    | _ -> failwith "Constraint type for scilab functions should not contains Depend case") l

let match_nth_arg nth t l = 
  List.filter (fun vers -> match vers with
    | Simple (Type_arrow (args, _)) -> Array.get args nth = t
    | _ -> false) l

let type_uminus = [
  Simple (Type_arrow ( [| Type_base Type_bool |], [| Type_base Type_bool |] ));
  Simple (Type_arrow ( [| Type_base Type_real |], [| Type_base Type_real |] ));
  Simple (Type_arrow ( [| Type_base Type_complex |], [| Type_base Type_complex |] ));
  
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_bool) |], [| Type_matrix (Type_base Type_bool) |] ));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_real) |], [| Type_matrix (Type_base Type_real) |] ));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_complex) |], [| Type_matrix (Type_base Type_complex) |] );)]
  
let type_plus = [
  Simple (Type_arrow ( [| Type_base Type_bool; Type_base Type_bool |], [| Type_base Type_bool |] ));
  Simple (Type_arrow ( [| Type_base Type_bool; Type_base Type_real |], [| Type_base Type_real |] ));
  Simple (Type_arrow ( [| Type_base Type_bool; Type_base Type_complex |], [| Type_base Type_complex |] ));
  Simple (Type_arrow ( [| Type_base Type_real; Type_base Type_bool |], [| Type_base Type_real |] ));
  Simple (Type_arrow ( [| Type_base Type_real; Type_base Type_real |], [| Type_base Type_real |] ));
  Simple (Type_arrow ( [| Type_base Type_real; Type_base Type_complex |], [| Type_base Type_complex |] ));
  Simple (Type_arrow ( [| Type_base Type_complex; Type_base Type_bool |], [| Type_base Type_complex |] ));
  Simple (Type_arrow ( [| Type_base Type_complex; Type_base Type_real |], [| Type_base Type_complex |] ));
  Simple (Type_arrow ( [| Type_base Type_complex; Type_base Type_complex |], [| Type_base Type_complex |] ));
  
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_bool); Type_base Type_bool |], [| Type_matrix (Type_base Type_bool ) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_bool); Type_base Type_real |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_bool); Type_base Type_complex |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_real); Type_base Type_bool |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_real); Type_base Type_real |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_real); Type_base Type_complex |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_complex); Type_base Type_bool |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_complex); Type_base Type_real |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_complex); Type_base Type_complex |], [| Type_matrix (Type_base Type_complex) |]));
  
  Simple (Type_arrow ( [| Type_base Type_bool; Type_matrix (Type_base Type_bool) |], [| Type_matrix (Type_base Type_bool) |]));
  Simple (Type_arrow ( [| Type_base Type_bool; Type_matrix (Type_base Type_real) |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_base Type_bool; Type_matrix (Type_base Type_complex) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_base Type_real; Type_matrix (Type_base Type_bool) |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_base Type_real; Type_matrix (Type_base Type_real) |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_base Type_real; Type_matrix (Type_base Type_complex) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_base Type_complex; Type_matrix (Type_base Type_bool) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_base Type_complex; Type_matrix (Type_base Type_real) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_base Type_complex; Type_matrix (Type_base Type_complex) |], [| Type_matrix (Type_base Type_complex) |]));
  
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_bool); Type_matrix (Type_base Type_bool) |], [| Type_matrix (Type_base Type_bool) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_bool); Type_matrix (Type_base Type_real) |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_bool); Type_matrix (Type_base Type_complex) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_real); Type_matrix (Type_base Type_bool) |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_real); Type_matrix (Type_base Type_real) |], [| Type_matrix (Type_base Type_real) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_real); Type_matrix (Type_base Type_complex) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_complex); Type_matrix (Type_base Type_bool) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_complex); Type_matrix (Type_base Type_real) |], [| Type_matrix (Type_base Type_complex) |]));
  Simple (Type_arrow ( [| Type_matrix (Type_base Type_complex); Type_matrix (Type_base Type_complex) |], [| Type_matrix (Type_base Type_complex) |]));
  
  Simple (Type_arrow ( [| Type_string; Type_string |], [| Type_string |] ));]




















