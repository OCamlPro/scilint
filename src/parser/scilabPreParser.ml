(* Pre-parsing file to deal with '..\n' and '...\n' *)

exception PreParserError

let add_flag = ref false

let add = ref ""

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
    if char1 = '/' & char2 = '/'
    then true
    else is_line_comment (index - 1) str

(* To deal with construction like '.. // cmt' *)
let rec get_lc_with_dotdot_index index str =
  if index < 1 
  then -1
  else
    let char1 = String.get str index in
    let char2 = String.get str (index - 1) in
    if char1 = '/' & char2 = '/' & index > 1
    then 
      let dot_dot_index = find_last_index (index - 2) str in
      (* Printf.printf "dotindex : %i\n" dot_dot_index; *)
      if dot_dot_index > 0
      then
        let dot1 = String.get str dot_dot_index in
        let dot2 = String.get str (dot_dot_index - 1) in
        if dot1 = '.' & dot2 = '.' 
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
      (* Printf.printf "index_dd : %i\n" index_dd; *)
      if index_dd = -1
      then
        if !add_flag
        then 
          begin
            let new_line = line ^ !add ^ "\n" in
            add_flag := false;
            add := "";
            Buffer.add_string buf new_line;
            pre_parse_aux buf ch
          end
        else 
          begin
            Buffer.add_string buf (line ^ "\n");
            pre_parse_aux buf ch
          end
      else
        begin
          add_flag := true;
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
      (* Printf.printf "dernier : %c; av_dernier : %c; avav_dernier : %c\n" last_char last_1_char last_2_char; *)
      if last_char = '.' & last_1_char = '.'
      then
        if last_2_char = '.'
        then
          begin
            (* "   \n" *)
            add_flag := true;
            add := !add ^ "   \n";
            let line_without = String.sub line 0 last_2_index in
            Buffer.add_string buf line_without;
            pre_parse_aux buf ch
          end
        else
          begin
            (* "  \n" *)
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
            let new_line = line ^ !add ^ "\n" in
            add_flag := false;
            add := "";
            Buffer.add_string buf new_line;
            pre_parse_aux buf ch
          end
        else 
          begin 
            Buffer.add_string buf (line ^ "\n");
            pre_parse_aux buf ch
          end
    else
      if last_index = 1
      then
        let last_char   = String.get line last_index and
            last_1_char = String.get line last_1_index in
        if last_char = '.' & last_1_char = '.'
        then
          begin
            (* "  \n" *)
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
              let new_line = line ^ !add ^ "\n" in
              add_flag := false;
              add := "";
              Buffer.add_string buf new_line;
              pre_parse_aux buf ch
            end
          else
            begin
              Buffer.add_string buf (line ^ "\n");
              pre_parse_aux buf ch
            end
      else
        begin 
          Buffer.add_string buf (line ^ "\n");
          pre_parse_aux buf ch
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
  pre_parse_aux buf ch
