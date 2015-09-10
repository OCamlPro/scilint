(*  OCamlPro Scilab Toolbox - OcSciLab, scope handling
 *  Copyright (C) 2014 - OCamlPro - Benjamin CANOU
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.
 *  The terms are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt *)

(** Stores the state of variables in the interpreter *)
module type S = sig

  (** Raised if {!resume} or {!frame} is called while no local scope
      has been opened inside the toplevel one. *)
  exception Toplevel

  (** Imperative state of the interpreter *)
  type state

  (** Variables inside a state, storing an optional global value and
      the optional local values for all opened scopes *)
  type variable

  (** Values stored in variables *)
  type value

  (** Stack frame identifiers *)
  type frame

  (** Builds an initial, empty state *)
  val init : unit -> state

  (** Retrieves / builds a variable from its name *)
  val var : state -> string -> variable

  (** Retrieves the name of a variable *)
  val name : variable -> string

  (** Opens a local scope *)
  val enter_scope : state -> frame -> unit

  (** Returns the current frame, may raise {!Toplevel} *)
  val frame : state -> frame

  (** Returns the number of frames in the stack *)
  val level : state -> int

  (** Closes a local scope *)
  val exit_scope : state -> unit

  (** Retrieve the value of a (local or global) variable in the given
      state *)
  val get : state -> variable -> value

  (** Retrieve the value of a (local or global) variable in the given
      state and claim its ownership for the copy on write mechanism *)
  val grab : state -> variable -> value

  (** Updates the value of a (local or global) variable in the given
      state, changes a global variable to a local one if {!global} was
      not called in this very scope *)
  val put : state -> variable -> value -> unit

  (** Unsets the value of a local variable in the given state *)
  val clear : state -> variable -> unit

  (** Unsets the global value of a variable in the given state *)
  val clear_global : state -> variable -> unit

  (** Sets the value of a variable in the previous scope *)
  val resume : state -> variable -> value -> unit

  (** Make the given variable global in the given scope, either
      retrieving the existing global value of the variable if it
      exists, copying the local one if it exists or [] *)
  val global : state -> variable -> unit

  (** Tells if a variable is local or global *)
  val is_global : state -> variable -> bool

end
