(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: ptmap.mli,v 1.8 2008-07-21 14:53:06 filliatr Exp $ i*)

(*s Maps over integers implemented as Patricia trees.
    The following signature is exactly [Map.S with type key = int],
    with the same specifications. *)

module type T = sig

    type (+'a) t

    type key

    val empty : 'a t

    val is_empty : 'a t -> bool

    val add : key -> 'a -> 'a t -> 'a t

    val find : key -> 'a t -> 'a

    val remove : key -> 'a t -> 'a t

    val mem :  key -> 'a t -> bool

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    val map : ('a -> 'b) -> 'a t -> 'b t

    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

end

include (T with type key = int)
