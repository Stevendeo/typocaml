(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2024 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCamlPro license.                                                     *)
(*                                                                        *)
(**************************************************************************)

let src = Logs.Src.create "typocaml"

module L = (val (Logs.src_log src))

let debug m = Fmt.kstr (fun s -> L.debug (fun p -> p "%s" s)) m

let enable_debug () =
  Logs.set_reporter @@
  Logs.format_reporter
    ~dst:Format.std_formatter
    ();
  Logs.Src.set_level src (Some Logs.Debug)

let disable_debug () =
  Logs.Src.set_level src None
