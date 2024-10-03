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

let (++) m str = Typocaml.add (String.lowercase_ascii str) () m

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
  let text = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium totam rem aperiam eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt Neque porro quisquam est qui dolorem ipsum quia dolor sit amet consectetur adipisci velit sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem Ut enim ad minima veniam quis nostrum exercitationem ullam corporis suscipit laboriosam nisi ut aliquid ex ea commodi consequatur Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur vel illum qui dolorem eum fugiat quo voluptas nulla pariatur" in
  let l = String.split_on_char ' ' text in
  List.fold_left (++) Typocaml.empty l

let autocomplete_i =
  Typocaml.get_autocompletions "i" dict

let autocomplete_ip =
  Typocaml.get_autocompletions "ip" dict

let autocomplete_ip' =
  Typocaml.get_autocompletions "p" autocomplete_i

let ok1 = Typocaml.find_opt ~max_typo:1 "voluptatam" dict
let ko1 = Typocaml.find_opt ~max_typo:1 "nuwwa" dict
let ok2 = Typocaml.find_opt ~max_typo:2 "nuwwa" dict

let ok3 = Typocaml.find_opt ~max_typo:1 "nulal" dict

let ca1 = Typocaml.autocomplete_autocorrect ~max_typo:0 "ca" dict
let ca2 = Typocaml.autocomplete_autocorrect ~max_typo:1 "ca" dict
let co1 = Typocaml.autocomplete_autocorrect ~max_typo:0 "co" dict

let () =
  pp dict;
  new_pp ();
  pp autocomplete_i;
  new_pp ();
  pp autocomplete_ip;
  new_pp ();
  pp autocomplete_ip';
  new_pp ();
  (* The two autocompletions should be equal *)
  Fmt.pf Format.std_formatter "%b" (autocomplete_ip = autocomplete_ip');
  new_pp ();
  (* Finding words in the tree. *)
  Fmt.pf Format.std_formatter "voluptatam(1): %a@." (pp_opt pp_unit) ok1;
  new_pp ();
  (* This one should not be found, it has two mistakes. *)
  Fmt.pf Format.std_formatter "nuwwa(1): %a@." (pp_opt pp_unit) ko1;
  new_pp ();
  (* With two typos, we can find it. *)
  Fmt.pf Format.std_formatter "nuwwa(2): %a@." (pp_opt pp_unit) ok2;
  new_pp ();
  Fmt.pf Format.std_formatter "nulal(1): %a@." (pp_opt pp_unit) ok3;
  new_pp ();
  (* Return all autocompletions of ca with 0 & 1 typos *)
  Fmt.pf Format.std_formatter "ca(0):[@.%a]@." pp_l ca1;
  Fmt.pf Format.std_formatter "ca(1):[@.%a]@." pp_l ca2;
  Fmt.pf Format.std_formatter "co(0):[@.%a]@." pp_l co1;
