(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCamlPro license.                                                     *)
(*                                                                        *)
(**************************************************************************)

exception Cannot_swap

let invalid_arg = Fmt.kstr invalid_arg "%s.%s" __MODULE__

(* The current character is the first character of the suffix *)
type t = {
  prefix: Bytes.t;
  suffix: Bytes.t;
}

let prefix_is_empty z = z.prefix = Bytes.empty

let suffix_is_empty z = z.suffix = Bytes.empty

let can_step z = not (suffix_is_empty z)

let current_char z =
  try Bytes.get z.suffix 0 with
  | Invalid_argument _ -> invalid_arg "current_char"

let of_string s =
  {prefix = Bytes.empty; suffix = Bytes.of_string s}

let to_string z =
  Fmt.str "%s%s" (Bytes.to_string z.prefix) (Bytes.to_string z.suffix)

let pp ppf z =
  Fmt.pf ppf "%s/%s" (Bytes.to_string z.prefix) (Bytes.to_string z.suffix)

let add_to_prefix c z =
  {z with prefix = Bytes.cat z.prefix (Bytes.make 1 c)}

let add_to_suffix c z =
  {z with suffix = Bytes.cat (Bytes.make 1 c) z.suffix}

let add ~prefix c z =
  if prefix
  then add_to_prefix c z
  else add_to_suffix c z

let step z =
  match current_char z with
  | exception Invalid_argument _ -> invalid_arg "zipper_step"
  | c ->
    {
      prefix = Bytes.cat z.prefix (Bytes.make 1 c);
      suffix = Bytes.sub z.suffix 1 (Bytes.length z.suffix - 1);
    }

let change_current_char c z =
  if suffix_is_empty z then invalid_arg "change_current_char";
  let s = z.suffix in
  {z with
   suffix = Bytes.(cat (make 1 c) (sub s 1 (length s - 1)))}

let remove_current_char z =
  if suffix_is_empty z then invalid_arg "remove_current_char";
  {z with suffix = Bytes.sub z.suffix 1 (Bytes.length z.suffix - 1)}

let swap_suffix z =
  let s = z.suffix in
  let len = Bytes.length s in
  if len < 2 then raise Cannot_swap;
  let c1 = Bytes.(make 1 @@ get s 0) in
  let c2 = Bytes.(make 1 @@ get s 1) in
  {
    z with
    suffix = Bytes.concat Bytes.empty [c2; c1; Bytes.(sub s 2 (length s - 2))]
  }

let swap_prefix z =
  if suffix_is_empty z || prefix_is_empty z then raise Cannot_swap;
  let p = z.prefix in
  let s = z.suffix in
  let plen = Bytes.length p in
  let slen = Bytes.length s in
  let c1 = Bytes.(make 1 @@ get p (plen - 1)) in
  let c2 = Bytes.(make 1 @@ get s 0) in
  {
    prefix = Bytes.(cat (sub p 0 (plen - 1)) c2);
    suffix = Bytes.(cat c1 (sub s 1 (slen - 1)));
  }

let swap ~prefix z =
  if prefix
  then swap_prefix z
  else swap_suffix z
