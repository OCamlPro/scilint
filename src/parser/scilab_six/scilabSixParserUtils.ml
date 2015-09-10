(*  OCamlPro Scilab Toolbox - Scilab 6 parser OCaml port
 *  Copyright (C) 2013 - OCamlPro - Michael LAPORTE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

open ScilabLocations
open ScilabParserAst
open ScilabLocations
open Lexing

(* Pre-parsing file to deal with '..\n' and '...\n' *)

exception PreParserError

let add_flag = ref false
let add = ref ""
let corrupt_zone = ref []
let corrupt = ref []
let cpt_line = ref 1
let list_white_space = [' '; '\012'; '\n'; '\r'; '\t']

(* To deal with '..       \n' *)
let rec find_last_index index str =
  if index <= 0 then 0
  else
    let char = String.get str index in
    if List.exists (fun x -> x = char) list_white_space
    then find_last_index (index - 1) str
    else index

let rec is_line_comment index str =
  if index < 1 then false
  else
    let char1 = String.get str index in
    let char2 = String.get str (index - 1) in
    if char1 = '/' && char2 = '/'
    then true
    else is_line_comment (index - 1) str

(* To deal with construction like '.. // cmt' *)
let rec get_lc_with_dotdot_index index str =
  if index < 1 
  then -1
  else
    let char1 = String.get str index in
    let char2 = String.get str (index - 1) in
    if char1 = '/' && char2 = '/' && index > 1
    then 
      let dot_dot_index = find_last_index (index - 2) str in
      (* Printf.printf "dotindex : %i\n" dot_dot_index; *)
      if dot_dot_index > 0
      then
        let dot1 = String.get str dot_dot_index in
        let dot2 = String.get str (dot_dot_index - 1) in
        if dot1 = '.' && dot2 = '.' 
        then 
          if dot_dot_index > 1
          then 
            let dot3 = String.get str (dot_dot_index - 2) in
            if dot3 = '.'
            then dot_dot_index - 2 
            else dot_dot_index - 1
          else dot_dot_index - 1
        else -1
      else -1
    else get_lc_with_dotdot_index (index - 1) str
   
let rec pre_parse_aux buf ch =
  try
    let line = input_line ch in
    (* Printf.printf "line : [%s]\n" line; *)
    let last_index   = find_last_index (String.length line - 1) line in
    let last_1_index = last_index - 1 in
    let last_2_index = last_index - 2 in
    if is_line_comment last_index line
    then
      let index_dd = get_lc_with_dotdot_index last_index line in
      if index_dd = -1
      then
        if !add_flag
        then 
          begin
            if List.length !corrupt <> 0 then corrupt_zone := (List.rev !corrupt)::!corrupt_zone;
            corrupt := [];
            let new_line = line ^ !add ^ "\n" in
            add_flag := false;
            add := "";
            incr cpt_line;
            incr cpt_line;
            Buffer.add_string buf new_line;
            pre_parse_aux buf ch
          end
        else 
          begin
            incr cpt_line;
            Buffer.add_string buf (line ^ "\n");
            pre_parse_aux buf ch
          end
      else
        begin
          if List.length !corrupt <> 0 then incr cpt_line;
          corrupt := (!cpt_line, index_dd)::!corrupt;
          add_flag := true;
          add := !add ^ "  \n";
          let line_without = String.sub line 0 index_dd in
          Buffer.add_string buf line_without;
          pre_parse_aux buf ch
        end
    else
    (* Printf.printf "last_index : %i\n" last_index; *)
    if last_index > 1
    then
      let last_char   = String.get line last_index and
          last_1_char = String.get line last_1_index and
          last_2_char = String.get line last_2_index in
      if last_char = '.' && last_1_char = '.'
      then
        if last_2_char = '.'
        then
          begin
            (* "..\n" *)
            if List.length !corrupt <> 0 then incr cpt_line;
            corrupt := (!cpt_line, last_2_index)::!corrupt;
            add_flag := true;
            add := !add ^ "  \n";
            let line_without = String.sub line 0 last_2_index in
            Buffer.add_string buf line_without;
            pre_parse_aux buf ch
          end
        else
          begin
            (* "...\n" *)
            if List.length !corrupt <> 0 then incr cpt_line;
            corrupt := (!cpt_line, last_1_index)::!corrupt;
            add_flag := true;
            add := !add ^ "   \n";
            let line_without = String.sub line 0 last_1_index in
            Buffer.add_string buf line_without;
            pre_parse_aux buf ch
          end
      else
        if !add_flag
        then 
          begin
            if List.length !corrupt <> 0 then corrupt_zone := (List.rev !corrupt)::!corrupt_zone;
            corrupt := [];
            let new_line = line ^ !add ^ "\n" in
            add_flag := false;
            add := "";
            incr cpt_line;
            incr cpt_line;
            Buffer.add_string buf new_line;
            pre_parse_aux buf ch
          end
        else 
          begin 
            incr cpt_line;
            Buffer.add_string buf (line ^ "\n");
            pre_parse_aux buf ch
          end
    else
      if last_index = 1
      then
        let last_char   = String.get line last_index and
            last_1_char = String.get line last_1_index in
        if last_char = '.' && last_1_char = '.'
        then
          begin
            (* "  \n" *)
            if List.length !corrupt <> 0 then incr cpt_line;
            corrupt := (!cpt_line, last_1_index)::!corrupt;
            add_flag := true;
            add := !add ^ "  \n";
            let line_without = String.sub line 0 last_1_index in
            Buffer.add_string buf line_without;
            pre_parse_aux buf ch
          end
        else
          if !add_flag
          then
            begin
              if List.length !corrupt <> 0 then corrupt_zone := (List.rev !corrupt)::!corrupt_zone;
              corrupt := [];
              let new_line = line ^ !add ^ "\n" in
              add_flag := false;
              add := "";
              incr cpt_line;
              incr cpt_line;
              Buffer.add_string buf new_line;
              pre_parse_aux buf ch
            end
          else
            begin
              incr cpt_line;
              Buffer.add_string buf (line ^ "\n");
              pre_parse_aux buf ch
            end
      else
        begin 
           if !add_flag
          then
            begin
              if List.length !corrupt <> 0 then corrupt_zone := (List.rev !corrupt)::!corrupt_zone;
              corrupt := [];
              let new_line = line ^ !add ^ "\n" in
              add_flag := false;
              add := "";
              incr cpt_line;
              incr cpt_line;
              Buffer.add_string buf new_line;
              pre_parse_aux buf ch
            end
           else
             begin
               incr cpt_line;
               Buffer.add_string buf (line ^ "\n");
               pre_parse_aux buf ch
             end
        end
  with 
    | End_of_file -> 
        if !add_flag
        then
          begin
            let new_line = !add in
            add_flag := false;
            add := "";
            Buffer.add_string buf new_line;
            Buffer.contents buf
          end
        else Buffer.contents buf
    | _ -> raise PreParserError

let pre_parse ch =
  let buf = Buffer.create 1024 in
  add := "";
  add_flag := false;
  let new_src = pre_parse_aux buf ch in
  new_src, List.rev !corrupt_zone

let list_line_corrupt_min = ref 0
let list_line_corrupt_max = ref 0
let list_line_corrupt = ref []
  
let init_var_corrupt list_zone = 
  if List.length list_zone <> 0 
  then
    let (min, _) = List.hd (List.hd list_zone) in
    let (max, _) = List.hd (List.hd (List.rev list_zone)) in
    list_line_corrupt_min := min;
    list_line_corrupt_max := max;
    let new_list = 
      List.map (fun zone -> 
        if List.length zone > 1 
        then 
          begin
            let (line, col) = List.hd zone in
            let new_zone = 
              List.map (fun (_, corrupt_col) -> 
                (line, corrupt_col + col)) (List.tl zone) in
            (line, col)::new_zone
          end
        else zone
      ) list_zone in
    list_line_corrupt := new_list

let uncorrupt_loc start_pos end_pos =
  (File start_pos.pos_fname,
   ((start_pos.pos_lnum,
     start_pos.pos_cnum - start_pos.pos_bol),
    (end_pos.pos_lnum,
     end_pos.pos_cnum - end_pos.pos_bol)))

let check_overlap start_pos end_pos list_bl = 
  let start_col = start_pos.pos_cnum - start_pos.pos_bol in
  let end_col = end_pos.pos_cnum - end_pos.pos_bol in
  let zone_overlaped = 
    List.filter (fun (line, col) -> 
        start_col < col && end_col > col) (List.hd list_bl) in
  List.length zone_overlaped <> 0  

let add_overlap start_pos end_pos nbr_line char_last_breakline list_bl =
  let start_line = start_pos.pos_lnum in
  let start_col = start_pos.pos_cnum - start_pos.pos_bol in
  let end_col = end_pos.pos_cnum - end_pos.pos_bol in
  let zone_overlaped = 
    List.filter (fun (line, col) -> 
        start_col < col && end_col > col) (List.hd list_bl) in
  if List.length zone_overlaped <> 0
  then 
    let nbr_ol_line = List.length zone_overlaped in
    (File start_pos.pos_fname,
     ((start_line + nbr_line,
       start_col - char_last_breakline),
      (start_line + nbr_line + nbr_ol_line,
       end_col - start_col - 1)))
  else uncorrupt_loc start_pos end_pos
  
let loc start_pos end_pos =
  let start_line = start_pos.pos_lnum in 
  let start_col = start_pos.pos_cnum - start_pos.pos_bol in
  if start_line < !list_line_corrupt_min || start_line > !list_line_corrupt_max
  then uncorrupt_loc start_pos end_pos
    else
      let list_line_br = 
        List.filter (fun zone -> 
            let (line, col) = List.hd zone in line = start_line
          ) !list_line_corrupt in
      let list_br = 
        List.filter (fun zone -> 
            let (line, col) = List.hd zone in col <= start_col
          ) list_line_br in
      if List.length list_line_br <> 0
      then
        if List.length list_br <> 0
        then 
          let list_char = 
            List.filter (fun (_, c) ->  c <= start_col) (List.hd list_br) in
          let nbr_line = List.length list_char in
          let (_, char_last_br) = List.hd (List.rev list_char) in
          if check_overlap start_pos end_pos list_br 
          then add_overlap start_pos end_pos nbr_line char_last_br list_line_br
          else
            (File start_pos.pos_fname,
             ((start_line + nbr_line,
               start_col - char_last_br),
              (start_line + nbr_line,
               (start_col - char_last_br) 
               + (end_pos.pos_cnum - end_pos.pos_bol) 
               - start_col)))
        else add_overlap start_pos end_pos 0 0 list_line_br
      else uncorrupt_loc start_pos end_pos

let descr ?(warns = []) ?(comment = []) cstr loc =
  { cstr ; comment ; loc ; meta = warns ; id = UUID.make () }

let string_descr ?warns str loc =
  descr ?warns str loc

let var_descr ?warns str loc =
  descr ?warns (Var (string_descr str loc)) loc
