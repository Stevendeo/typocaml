(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCamlPro license.                                                     *)
(*                                                                        *)
(**************************************************************************)

(* let () = Typocaml.Log.activate_debug () *)

let (++) m str = Typocaml.add str () m

let pp_unit ppf () = Fmt.pf ppf "x"

let pp t =
  Typocaml.pp ~pp_data:pp_unit Format.std_formatter t

let pp_l ppf l =
  List.iter
    (fun (word, (), typos) -> Fmt.pf ppf "%s(%i);@." word typos)
    l

let new_pp () =
  Fmt.pf Format.std_formatter "@.--@."

let pp_opt pp ppf = function
  | None -> Fmt.pf ppf "<none>"
  | Some v -> pp ppf v

let dict =
  Typocaml.empty
  ++ "Hello"
  ++ "Hel"
  ++ "Hell"
  ++ "Tadah"
  ++ "Helo"
  ++ "Tadaa"
  ++ "Habut"

let autocomplete_h =
  Typocaml.get_autocompletions "H" dict

let autocomplete_he =
  Typocaml.get_autocompletions "He" dict

let autocomplete_he' =
  Typocaml.get_autocompletions "e" autocomplete_h

let hallo1  = Typocaml.find_opt ~max_typo:1 "Hallo" dict
let hellww1 = Typocaml.find_opt ~max_typo:1 "Hellww" dict
let hellww2 = Typocaml.find_opt ~max_typo:2 "Hellww" dict
let helol1 = Typocaml.find_opt ~max_typo:1 "Helol" dict

let he = Typocaml.autocomplete_autocorrect ~max_typo:0 "He" dict
let he'= Typocaml.autocomplete_autocorrect ~max_typo:1 "He" dict

let () =
  pp dict;
  new_pp ();
  pp autocomplete_h;
  new_pp ();
  pp autocomplete_he;
  new_pp ();
  pp autocomplete_he';
  new_pp ();
  (* The two autocompletions should be equal *)
  Fmt.pf Format.std_formatter "%b" (autocomplete_he = autocomplete_he');
  new_pp ();
  (* Finding words in the tree. *)
  Fmt.pf Format.std_formatter "Hallo(1): %a@." (pp_opt pp_unit) hallo1;
  new_pp ();
  (* This one should not be found, it has two mistakes. *)
  Fmt.pf Format.std_formatter "Hellww(1): %a@." (pp_opt pp_unit) hellww1;
  new_pp ();
  (* With two typos, we can find it. *)
  Fmt.pf Format.std_formatter "Hellww(2): %a@." (pp_opt pp_unit) hellww2;
  new_pp ();
  Fmt.pf Format.std_formatter "Helol(1): %a@." (pp_opt pp_unit) helol1;
  new_pp ();
  (* Return all autocompletions of He with 0 & 1 typos *)
  Fmt.pf Format.std_formatter "He(0):[@.%a]@." pp_l he;
  Fmt.pf Format.std_formatter "He(1):[@.%a]@." pp_l he';
