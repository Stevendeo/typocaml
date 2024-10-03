(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCamlPro license.                                                     *)
(*                                                                        *)
(**************************************************************************)

(** The main TypoCaml module. *)

(** A typocaml map. *)
type 'a t

(** An empty map. *)
val empty : 'a t

(** Adds a string to the map. *)
val add : string -> 'a -> 'a t -> 'a t

(** Checks that a string has been registered in the map. *)
val mem : string -> 'a t -> bool

(** [find_opt ?max_typo word t]

    Searches for [word] in the autocompletion map [t]. The word
    can have at least [max_typo] typos. If multiple words match
    with [word], returns one of the closest word from [word]. *)
val find_opt : ?max_typo:int -> string -> 'a t -> 'a option

(** [autocomplete key t]

    Returns all the exact autocompletions of [key] registered in [t]. *)
val autocomplete : string -> 'a t -> (string * 'a) list

(** [get_autocompletions key t]

    Returns a sub autocompletion map containing all the suffixes of [key]
    in [t] *)
val get_autocompletions : string -> 'a t -> 'a t

(** [autocorrect ~max_typo key t]

    Returns all the words with at most [max_typo] typos. *)
val autocorrect : max_typo:int -> string -> 'a t -> (string * 'a * int) list

(** [autocomplete_autocorrect ~max_type key t]

    Returns all the words starting with [key], with at most [max_typo] typos. *)
val autocomplete_autocorrect :
  max_typo:int -> string -> 'a t -> (string * 'a * int) list

(** Returns the list of words registered in an autocompletion map. *)
val to_list : 'a t -> (string * 'a) list

(** Pretty prints a map. *)
val pp :
  pp_data:(Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit

module Log : sig

  (** Enables the log messages of the library *)
  val enable_debug : unit -> unit

  (** Disables the log messages of the library *)
  val disable_debug : unit -> unit

end
