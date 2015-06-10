(*  OCamlPro Scilab Toolbox - OcSciLab, display of values and messages
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open InterpCore

type interp_message =
  | Warning of ScilintWarning.warning
  | Werror of ScilintWarning.warning
  | Unbound_overloading of (Dispatcher.overloading * Values.rtt list)
  | Result of string * Values.value
  | Hint of string
  | Generic of string
  | Error of string
  | Located of Ast.loc * interp_message

exception Interp_error of interp_message

let print_value ppf value =
  let open Values in
  let print fmt = Format.fprintf ppf fmt in
  let print_poly
    : type coeff. bool -> (?wrap:bool -> coeff -> unit) -> coeff -> coeff -> coeff poly -> unit
    = fun wrap print_coeff nil one p ->
      if poly_variable p = "$" then begin
        let empty = ref true in
        if poly_length p = 0 then
          if wrap then print "($ * 0)" else print "$ * 0"
        else
          let v0 = poly_get p 1 in
          if wrap then print "(" ;
          if v0 <> nil then begin
            print_coeff v0 ;
            empty := false
          end ;
          for i = 2 to poly_length p do
            let v = poly_get p i in
            if v <> nil then begin
              if not !empty then print " + " ;
              if v <> one then (print_coeff ~wrap:true v ; print " * ") ;
              print "$" ;
              if i <> 2 then print " ** %d" (i - 1) ;
              empty := false
            end
          done ;
          if wrap then print ")"
      end else begin
        print "poly ([" ;
        let s = poly_length p in
        for i = 1 to s do
          let v = poly_get p i in
          print_coeff v ;
          if i <> s then print ", "
        done ;
        print "], %S, \"coeff\")" (poly_variable p)
      end in
  let rec print_value ?(wrap = false) = function
    | V (Primitive, s) -> print "fptr (%S)" s
    | V (Macro, { Ast.name = { Ast.cstr }}) ->
      print "function (%S)" (State.name cstr)
    | V (Eye (Number Real), 1.) -> print "eye ()"
    | V (Eye t, v) when wrap ->
      print "(eye () * " ; print_value ~wrap:true (V (Single t, v)) ; print ")"
    | V (Eye t, v) -> print "eye () * " ; print_value ~wrap:true (V (Single t, v))
    | V (Single String, s) -> print "%S" s
    | V (Single Bool, true) -> print "T"
    | V (Single Bool, false) -> print "F"
    | V (Atom, ()) -> print "[]"
    | V (Null, ()) -> print "null ()"
    | V (Single Int8, v) -> print "int8 (%i)" v
    | V (Single Int16, v) -> print "int16 (%i)" v
    | V (Single Int32, v) -> print "int32 (%i)" v
    | V (Single Uint8, v) -> print "uint8 (%i)" v
    | V (Single Uint16, v) -> print "uint16 (%i)" v
    | V (Single Uint32, v) -> print "uint32 (%i)" v
    | V (Single (Poly Real), p) ->
      print_poly wrap
        (fun ?wrap v -> print_value ?wrap (V (Single (Number Real), v)))
        0. 1. p
    | V (Single (Poly Complex), p) ->
      print_poly wrap
        (fun ?wrap v -> print_value ?wrap (V (Single (Number Complex), v)))
        (0., 0.) (1., 0.) p
    | V (Single (Number Real), v) -> print "%g" v
    | V (Single (Number Complex), (0., 1.)) -> print "%%i"
    | V (Single (Number Complex), (0., im)) when wrap -> print "(%g * %%i)" im
    | V (Single (Number Complex), (0., im)) -> print "%g * %%i" im
    | V (Single (Number Complex), (re, 0.)) -> print "%g" re
    | V (Single (Number Complex), (re, im)) when wrap -> print "(%g + %g * %%i)" re im
    | V (Single (Number Complex), (re, im)) -> print "%g + %g * %%i" re im
    | V (Vlist, l) ->
      print "list (" ;
      let s = vlist_length l in
      for i = 1 to s do
        print_value (view (vlist_get l i)) ;
        if i <> s then print ", "
      done ;
      print ")"
    | V (Tlist n, l) ->
      print "tlist (@[" ;
      begin match tlist_fields l with
        | [] -> print "'%s'" n
        | fs ->
          print "[ '%s'" n ;
          List.iter (print ", '%s'") fs ;
          print " ]"
      end ;
      let s = tlist_length l in
      for i = 1 to s do
        print ",@ " ;
        print_value (view (tlist_get_by_index l i)) ;
      done ;
      print "@])"
    | V (Mlist n, l) ->
      print "mlist (@[" ;
      begin match mlist_fields l with
        | [] -> print "'%s'" n
        | fs ->
          print "[ '%s'" n ;
          List.iter (print ", '%s'") fs ;
          print " ]"
      end ;
      let s = mlist_length l in
      for i = 1 to s do
        print ",@ " ;
        print_value (view (mlist_get_by_index l i)) ;
      done ;
      print "@])"
    | V (Matrix tag, m) ->
      print "[ @[<v 0>" ;
      let w, h = matrix_size m in
      for y = 1 to h do
        print "@[<hov 0>" ;
        for x = 1 to w do
          print_value (V (Single tag, matrix_get m x y)) ;
          if x <> w then print ",@ "
        done ;
        print "@]" ;
        if y <> h then print " ;@ "
      done ;
      print "@] ]" ;
    | V (Sparse tag, m) ->
      print "[ @[<v 0>" ;
      let w, h = sparse_size m in
      for y = 1 to h do
        print "@[<hov 0>" ;
        for x = 1 to w do
          print_value (V (Single tag, sparse_get m x y)) ;
          if x <> w then print ",@ "
        done ;
        print "@]" ;
        if y <> h then print " ;@ "
      done ;
      print "@] ]" ;
    | V (Handle, _) ->
      print "handle"
  in
  print_value (view value)

let string_of_value value =
  let buf = Buffer.create 100 in
  Format.(fprintf (formatter_of_buffer buf)) "%a@?" print_value value ;
  Buffer.contents buf

let rec string_of_type rtt =
  let open Values in
  match rtt with
  | T (Single (Number Real)) -> "real"
  | T (Single (Number Complex)) -> "complex"
  | T (Single (Poly Real)) -> "real polynom"
  | T (Single (Poly Complex)) -> "complex polynom"
  | T (Single String) -> "string"
  | T (Single Bool) -> "boolean"
  | T (Single Int8) -> "int8"
  | T (Single Int16) -> "int16"
  | T (Single Int32) -> "int32"
  | T (Single Uint8) -> "uint8"
  | T (Single Uint16) -> "uint16"
  | T (Single Uint32) -> "uint32"
  | T (Eye itag) -> string_of_type (T (Single itag)) ^ " eye"
  | T (Matrix itag) -> string_of_type (T (Single itag)) ^ " matrix"
  | T (Sparse itag) -> string_of_type (T (Single itag)) ^ " sparse matrix"
  | T (Vlist) -> "list"
  | T (Tlist n) -> n
  | T (Mlist n) -> n
  | T (Macro) -> "function"
  | T (Primitive) -> "fptr"
  | T (Handle) -> "handle"
  | T (Atom) -> "empty matrix"
  | T (Null) -> "null"

let error msg =
  raise (Interp_error msg)

let rec format loc msg =
  let open ScilintWarning in
  let open Dispatcher in
  let overloading_name = function
    | Colon -> "range operation"
    | Matrix_horizontal_collation -> "horizontal matrix concatenation"
    | Matrix_vertical_collation -> "vertical matrix concatenation"
    | Injection -> "injection"
    | Extraction -> "extraction"
    | Recursive_extraction -> "recursive extraction"
    | Unary op -> Ast.string_of_unop op
    | Binary op -> Ast.string_of_op op
    | Function name -> "primitive " ^ name
    | Print -> "printing" in
  let hint overloading rtts = try
      [ loc, Hint ("you can try redefining "
                   ^ Dispatcher.overloading_notation overloading rtts) ]
    with Failure _ -> [] in
  match msg with
  | Unbound_overloading (overloading, []) ->
    [ loc, Unrecovered (overloading_name overloading
                        ^ " does not work without an argument") ]
    @ hint overloading []
  | Unbound_overloading (overloading, [ rtt ]) ->
    [ loc, Unrecovered (overloading_name overloading
                        ^ " does not work on a value of type "
                        ^ string_of_type rtt) ]
    @ hint overloading [ rtt ]
  | Unbound_overloading (overloading, [ rtt1 ; rtt2 ]) ->
    [ loc, Unrecovered (overloading_name overloading
                        ^ " does not work between values of types "
                        ^ string_of_type rtt1
                        ^ " and "
                        ^ string_of_type rtt2) ]
    @ hint overloading [ rtt1 ; rtt2 ]
  | Unbound_overloading (overloading, rtts) ->
    let rec names = function
      | [] -> assert false
      | [ one ; last ] -> [ string_of_type one ; " and " ; string_of_type last ]
      | one :: rest -> string_of_type one :: ", " :: names rest
    in
    [ loc, Unrecovered (String.concat ""
                          (overloading_name overloading
                           :: " does not work on values of types "
                           :: names rtts)) ]
    @ hint overloading rtts
  | Warning w ->
    [ loc, Warning w ]
  | Werror w ->
    [ loc, Werror w ]
  | Generic n ->
    [ loc, Unrecovered n ]
  | Error n ->
    [ loc, Unrecovered n ]
  | Hint n ->
    [ loc, Hint n ]
  | Located ((ScilabLocations.Nowhere, _), msg) ->
    format loc msg
  | Located ((source, bounds) as loc', msg) ->
    if loc <> loc' then
      format (ScilabLocations.Call (loc, source), bounds) msg
    else
      format loc msg
  | Result (var, v) ->
    [ loc, Generic ("30", None, fun ppf -> Format.fprintf ppf "%s = %a" var print_value v) ]
  
let message msg =
  let msgs = format (ScilabLocations.Nowhere, ((0, 0), (0, 0))) msg in
  ScilintWarning.output_messages !ScilintOptions.format msgs stderr

let messages msgs =
  let msgs = List.(map (format (ScilabLocations.Nowhere, ((0, 0), (0, 0)))) msgs |> flatten) in
  ScilintWarning.output_messages !ScilintOptions.format msgs stderr
