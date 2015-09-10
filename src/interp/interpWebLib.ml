open InterpLib

let () =
  register_library (fun state lib ->
      register_function lib state "prompt" (string @-> string)
        (fun msg ->
           Js.Opt.case
             (Dom_html.window##prompt (Js.string msg, Js.string ""))
             (fun () -> "")
             (fun s -> Js.to_string s)) ;
      register_function lib state "alert" (string @-> void)
        (fun msg -> Dom_html.window##alert (Js.string msg)) ;
    )
