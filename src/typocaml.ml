(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCamlPro license.                                                     *)
(*                                                                        *)
(**************************************************************************)

module Log = Log

let char_to_str c = String.make 1 c

module M = Map.Make (struct type t = char let compare = compare end)

(* A map of maps. A character is mapped to a map of characters, etc.
   If the list of character is mapped to something, returns it. *)
type 'a t = {
  children : 'a t M.t;
  data : 'a option
}

(* Pretty printer *)

let pp_opt pp ppf = function
  | None -> Fmt.pf ppf ""
  | Some d -> Fmt.pf ppf " %a" pp d

let rec pp_children ~pp_data ppf t =
  if M.is_empty t.children then
    Fmt.pf ppf "%a" (pp_opt pp_data) t.data
  else
    Fmt.pf ppf
      "%a@,  @[<v>%a@]"
      (pp_opt pp_data) t.data
      (pp_map ~pp_data) t.children

and pp_map ~pp_data ppf m =
  let first = ref true in
  M.iter
    (fun c t ->
       if not !first then
         Fmt.pf ppf "@,"
       else
         first := false;
       Fmt.pf ppf "| %c ->%a" c (pp_children ~pp_data) t)
    m

let pp ~pp_data ppf t =
  Fmt.pf ppf
    "@[<v>%a@]"
    (pp_map ~pp_data) t.children

let pp_no_data ppf t =
  pp ~pp_data:(fun _ -> ignore) ppf t

let empty = {
  children = M.empty;
  data = None
}

let update c n m = {m with children = M.update c n m.children }

(* word_to_char_sub "string" returns the tuple ('s', "tring").
   Asserts input is not "" *)
let word_to_char_sub w =
  assert (w <> "");
  w.[0], String.sub w 1 (String.length w - 1)

let add (word : string) (token : 'a) (r : 'a t) : 'a t =
  let rec _add subr w : 'a t =
    match w with
      "" ->
      (* We add the token on the node after the word *)
      {subr with data = Some token}
    | _ -> (
        let c,w' = word_to_char_sub w in
        update
          c
          (function
            | None -> Some (_add empty w')
            | Some map -> Some (_add map w'))
          subr
      )
  in
  _add r word

(* Returns the map of possible autocompletions *)
let get_autocompletions (key : string) (r : 'a t) : 'a t =
  let rec _get subr w : 'a t =
    match w with
      "" -> subr
    | _ -> (
        let c,w' = word_to_char_sub w in
        match M.find_opt c subr.children with
          None -> empty
        | Some map -> _get map w'
      )
  in
  _get r key

(* Return a list of possible autocompletions. A word badly written can be found if
   there is only [max_typo] typos. *)
let get_clever_autocompletions ~max_typo (word : string) (r : 'a t) : (string * 'a t * int) list =
  (* takes a "typo" counter counting typos and returns autocompletions.
     If [inswap] = true, already testing swapping letters so won't try to swap again.
     [z] is the current word represented as a zipper
     [subr] is the current map
     [acc] is the accumulated list of maps that matches the key word *)
  let rec autofix_strategies ~inswap z typo_cpt subr acc =
    (* First strategy: discard the letter *)
    Log.debug "First strategy: discard the letter";
    let acc = _get acc (typo_cpt + 1) subr (Str_zipper.remove_current_char z) in
    (* Second strategy: swap letters *)
    Log.debug "Second strategy: swap letters ";
    let acc =
      if inswap then (* No possible swapping *)
        begin
          Log.debug "We already swapped: no swapping";
          acc
        end else
        match Str_zipper.swap ~prefix:false z with
        | exception Str_zipper.Cannot_swap -> (* Impossible to swap *)
          Log.debug "Impossible to swap";
          acc
        | new_zip ->
          _get ~inswap:true acc (typo_cpt + 1) subr new_zip
    in
    (* Third and fourth: checking subtree *)
    M.fold
      (fun (c' : char) (subr' : 'a t) (acc : (string * 'a t * int) list) ->
         (* First possible fix : letter replacement *)
         Log.debug "Begin strategies with %c" c';
         let acc =
           if Str_zipper.current_char z = c' then
             acc
           else
             let next_zip = Str_zipper.change_current_char c' z in
             let next_zip = Str_zipper.step next_zip in
             _get acc (typo_cpt + 1) subr' next_zip
         in
         (* Second possible fix : Missing letter *)
         Log.debug "Begin fourth strategy";
         let next_zip = Str_zipper.add ~prefix:true c' z in
         let acc = _get acc (typo_cpt + 1) subr' next_zip in
         Log.debug "End strategies with %c" c';
         acc
      )
      subr.children
      acc
  and  _get ?(inswap=false) subrs_acc typo_cpt subr str_zipper
    : (string * 'a t * int) list =
    Log.debug "Current word: %a -- %i typos" Str_zipper.pp str_zipper typo_cpt;
    Log.debug "Current tree:@. %a" pp_no_data subr;
    if typo_cpt > max_typo then begin
      Log.debug "Reached the max number of typos, giving up";
      subrs_acc
    end else if Str_zipper.can_step str_zipper then begin
      Log.debug "Step forward";
      let curr_char = Str_zipper.current_char str_zipper in
      match M.find_opt curr_char subr.children with
      | None -> (* There is either no word, or a typo. *)
        Log.debug "Typo detected, no followup with %c in %a"
          curr_char pp_no_data subr;
        autofix_strategies ~inswap str_zipper typo_cpt subr subrs_acc
      | Some map -> (* There is a corresponding word. *)
        let some_words = _get subrs_acc typo_cpt map (Str_zipper.step str_zipper) in
        autofix_strategies ~inswap str_zipper typo_cpt subr some_words
    end else begin
      let str = Str_zipper.to_string str_zipper in
      Log.debug "Reached the end of the zipper %s, returning the tree@. %a"
        str pp_no_data subr
      ;
      (str, subr, typo_cpt) :: subrs_acc
    end
  in
  Log.debug "Start search for %s" word;
  let res = _get [] 0 r (Str_zipper.of_string word) in
  Log.debug "End search.";
  res

let to_list (r : 'a t) : (string * 'a) list =
  let rec _iter subword wlist r : (string * 'a) list =
    M.fold
      (fun (c : char) (s : 'a t) (acc : (string * 'a) list) ->
         let new_word = subword ^ (char_to_str c) in
         let acc =
           match s.data with
             Some bind -> (new_word, bind) :: acc
           | None -> acc in
         (_iter new_word acc s)
      )
      r.children
      wlist
  in
  _iter "" [] r

let autocomplete key t =
  t
  |> get_autocompletions key
  |> to_list

let autocorrect ~max_typo key t =
  let tl = get_clever_autocompletions ~max_typo key t in
  let tl =
    List.filter_map (fun (s, t, i) ->
        match t.data with
        | None -> None
        | Some d -> Some (s, d, i)
      ) tl
  in
  List.fast_sort (fun (_, _, i) (_, _, i') -> i - i') tl

let find_opt ?(max_typo = 0) (key : string) (t : 'a t) : 'a option =
  if max_typo <= 0 then
    (get_autocompletions key t).data
  else
    match autocorrect ~max_typo key t with
    | (_, data, _) :: _ -> Some data
    | _ -> None

let autocomplete_autocorrect ~max_typo key t =
  let tl = get_clever_autocompletions ~max_typo key t in
  let l =
    List.fold_left
      (fun acc (str, a, t) ->
         let l = to_list a in
         let l = List.map (fun (s, d) -> str ^ s, d, t) l
         in
         l @ acc
      )
      [] tl
  in
  List.fast_sort (fun (_, _, i) (_, _, i') -> i - i') l

let mem (word : string) (r : 'a t) : bool =
  find_opt word r <> None
