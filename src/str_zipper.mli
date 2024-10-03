(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCamlPro license.                                                     *)
(*                                                                        *)
(**************************************************************************)

(** A zipper for characters *)

exception Cannot_swap

(** The zipper, represented by a pair of bytes. *)
type t

(** Builds a zipper from a string. *)
val of_string : string -> t

(** Returns the string corresponding to the zipper's content. *)
val to_string : t -> string

(** Returns [true] if the zipper can step forward. Returns false otherwise. *)
val can_step : t -> bool

(** Makes a step forward in the zipper.
    Raises [Invalid_argument] if the zipper cannot make a step forward. *)
val step : t -> t

(** Returns the current character of the zipper.
    Raises [Invalid_argument] if the zipper reached the end. *)
val current_char : t -> char

(** Prints a zipper. Only use for debug. *)
val pp : Format.formatter -> t -> unit

(** {2 Operations on zippers} *)

(** [swap ~prefix z]

    Swaps the current character with the previous character if
    [prefix] is set to [true], or with the next character if
    [prefix] is set to [false].
    Raises [Cannot_swap] if there is not enough characters to swap. *)
val swap : prefix:bool -> t -> t

(** Removes the current character of the zipper.
    Raises [Invalid_argument] if the zipper is empty. *)
val remove_current_char : t -> t

(** [add ~prefix char z]

    Adds a character to the zipper. If [prefix] is [true], adds
    [char] before the current character. Otherwise, the current
    character becomes [char]. *)
val add : prefix:bool -> char -> t -> t

(** Replaces the current character.
    Raises [Invalid_argument] if the zipper reached the end. *)
val change_current_char : char -> t -> t
