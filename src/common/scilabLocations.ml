(*  OCamlPro Scilab Toolbox - Common types and code for displaying warning
 *  Copyright (C) 2013 - OCamlPro - Benjamin CANOU, Michael LAPORTE
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** Code location (source, (start, end))) with the source, the
    starting code point (incl.) and ending code point (excl). *)
type loc = source * bounds

(** Code boundaries between two code points (start, stop) *)
and bounds = point * point

(** A single code point (line, column) where line >= 1, column >= 0 *)
and point = int * int

(** The source from which the code has been parsed *)
and source =
  | String of string * string (** (name, raw input string) *)
  | File of string (** file name from its path *)
  | Eval_string of loc (** recursive source for eval strings *)
  | Forged (** ghost source *)
