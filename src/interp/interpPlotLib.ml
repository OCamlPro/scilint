open InterpLib
open InterpCore.Values

type kind = [ `L | `B ]

type archi_plot = {
  mutable style : Archimedes.style;
  mutable xvalues : float list;
  mutable yvalues : float list;
  mutable title : string option;
  mutable xlabel : string option;
  mutable ylabel : string option;
  mutable grid : bool}
type archi_plots = {
  mutable plots : archi_plot list;
  mutable updated : bool }
let all_plots = { plots = []; updated = false}

let () =
  register_library (fun state lib ->
      register_function lib state "clf" (void @-> null)
        (fun () ->
	         all_plots.plots <- [];
	         (*plots.updated <- true;*)
        ) ;
      register_function lib state "plot" (matrix real @* matrix real @-> null)
        (fun xMat yMat ->
	         let w, h = matrix_size xMat in
	         let xval = ref []  and yval = ref [] in
	         for i=1 to w do
	           for j=1 to h do
	             xval := matrix_get xMat i j :: !xval ;
	             yval := matrix_get yMat i j :: !yval ;
	           done;
	         done;
	        let open InterpLib in
	        all_plots.plots <- 
	          all_plots.plots @ [{style =`Lines; xvalues = !xval; yvalues = !yval;
			                                    grid=false; title=Some ""; xlabel=Some ""; ylabel=Some ""}] ;
	        all_plots.updated <- true) ;
      register_function lib state "bar" (matrix real @* matrix real @-> null)
        (fun xMat yMat ->
	         let w, h = matrix_size xMat in
	         let xval = ref []  and yval = ref [] in
	         for i=1 to w do
	           for j=1 to h do
	             xval := !xval @ [matrix_get xMat i j];
	             yval := !yval @ [matrix_get yMat i j];
	           done;
	         done;
	        let open InterpLib in
	        all_plots.plots <- 
	          all_plots.plots @ [{style =(`Bars 0.1); xvalues = !xval; yvalues = !yval;
			                                    grid=false; title=Some ""; xlabel=Some ""; ylabel=Some ""}] ;
	        all_plots.updated <- true) ;
      register_function lib state "xtitle" (string @* opt string @* opt string @-> null)
        (fun title xlabel ylabel ->
	         let hd = List.hd all_plots.plots in
	         let tl = List.tl all_plots.plots in
	         let new_hd = match xlabel, ylabel with
	           | None, None -> {hd with title=Some title}
	           | Some x, Some y -> {hd with xlabel=Some x; ylabel=Some y; title=Some title}
	           | None, Some y -> {hd with ylabel=Some y; title=Some title}
	           | Some x, None -> {hd with xlabel=Some x; title=Some title} in
	         all_plots.plots <- new_hd :: tl;
	         all_plots.updated <- true) ;
      register_function lib state "grid" (void @-> null)
        (fun () -> 
	         let hd = List.hd all_plots.plots in
	         let tl = List.tl all_plots.plots in
	         let new_hd = {hd with grid = not hd.grid} in
	         all_plots.plots <- new_hd :: tl;
	         all_plots.updated <- true)
    )
