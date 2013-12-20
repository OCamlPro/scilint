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
