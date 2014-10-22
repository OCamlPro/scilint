(*  OCamlPro Scilab Toolbox - OcSciLab, core interpret structures
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** Instanciation of all the modules used by the interpreter *)

module rec Ast
  : ScilabAst.S
    with type loc = Parameters.loc
     and type symbol = Parameters.symbol
     and type meta = Parameters.meta = struct
  include ScilabAst.Make (Parameters)
end
and Parameters
  : ScilabAst.Parameters
    with type loc = ScilabParserAst.loc
     and type symbol = State.variable
     and type meta = unit = struct
  type loc = ScilabParserAst.loc
  type symbol = State.variable
  type meta = unit
  let ghost_loc = ScilabParserAst.ghost_loc
  let ghost_meta = ()
end
and Values
  : InterpValues.S with type macro = Ast.defun_params = struct
  module Parameters = struct type macro = Ast.defun_params end
  include InterpValuesPureOCaml.Make (Parameters)
end
and State
  : InterpState.S with type value := Values.value = struct
  include InterpStatePureOCaml.Make (Values)
end
and Dispatcher
  : InterpDispatcher.S with type rtt := Values.rtt = struct
  include InterpDispatcher.Make (Values)
end

module AstUtils = struct
  include ScilabAstUtils.Make (Ast)

  let from_parser state =
    let module ConverterParameters = struct
      module From = ScilabParserAst.Ast
      module To = Ast
      let loc loc = loc
      let symbol symbol = State.var state symbol
      let meta _ = ()
    end in
    let module FromParser =
      ScilabAstConverter.Make
        (ScilabParserAst.Ast) (Ast)
        (ConverterParameters) in
    FromParser.convert_ast
end

type primitive =
  { call : (Ast.var option * Values.value) list -> Values.value list ;
    takes : Dispatcher.matcher list ;
    returns : Dispatcher.matcher list ;
    name : string }

type lib =
  primitive Dispatcher.table

type state =
    State.state
