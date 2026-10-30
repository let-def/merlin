(* {{{ COPYING *(

     This file is part of Merlin, an helper for ocaml editors

     Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                                Thomas Refis  <refis.thomas(_)gmail.com>
                                Simon Castellan  <simon.castellan(_)iuwt.fr>

     Permission is hereby granted, free of charge, to any person obtaining a
     copy of this software and associated documentation files (the "Software"),
     to deal in the Software without restriction, including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
     sell copies of the Software, and to permit persons to whom the Software is
     furnished to do so, subject to the following conditions:

     The above copyright notice and this permission notice shall be included in
     all copies or substantial portions of the Software.

     The Software is provided "as is", without warranty of any kind, express or
     implied, including but not limited to the warranties of merchantability,
     fitness for a particular purpose and noninfringement. In no event shall
     the authors or copyright holders be liable for any claim, damages or other
     liability, whether in an action of contract, tort or otherwise, arising
     from, out of or in connection with the software or the use or other dealings
     in the Software.

   )* }}} *)

module R = Parser_recovery

type kind = ML | MLI
(*| MLL | MLY*)

type tree =
  [ `Interface of Parsetree.signature | `Implementation of Parsetree.structure ]

type t =
  { kind : kind; tree : tree; errors : exn list; lexer : Mreader_lexer.t }

(* The first symbol shifted by the cheapest strategy of the recovery from
   [env], for the explanation of an error *)
let shifted env =
  let rec first tactic =
    let stop = R.tactic_offsets (tactic + 1) in
    let rec action i =
      if i >= stop then None
      else
        let a = R.tactic_actions i in
        match a land 3 with
        | 1 -> Parser_printer.symbol_of_index (a lsr 2)
        | 2 -> (
          match first (a lsr 2) with
          | None -> action (i + 1)
          | sym -> sym)
        | _ -> action (i + 1)
    in
    action (R.tactic_offsets tactic)
  in
  match R.strategies env with
  | [] -> None
  | strategy :: _ -> first strategy.R.tactic

let errors_ref = ref []

let on_error env token =
  let explanation = Mreader_explain.explain env token (shifted env) in
  errors_ref := Mreader_explain.Syntax_explanation explanation :: !errors_ref

(* A semantic action rejected the input *)
let on_exception _env _token exn = Msupport.raise_error exn

(* The recovery needs the end of input *)
let with_eof initial_pos tokens =
  match List.rev tokens with
  | (Parser_raw.EOF, _, _) :: _ -> tokens
  | [] -> [ (Parser_raw.EOF, initial_pos, initial_pos) ]
  | (_, _, endp) :: _ -> tokens @ [ (Parser_raw.EOF, endp, endp) ]

(* The recovery is guided by the indentation: the tokens of a line more
   indented than the enclosing ones should end up in the same construct *)
let parse entry tokens initial_pos =
  let tokens = with_eof initial_pos tokens in
  R.parse ~on_error ~on_exception R.indentation (entry initial_pos)
    (List.to_seq tokens)

let run_parser warnings lexer kind =
  Msupport.catch_errors warnings errors_ref @@ fun () ->
  let tokens = Mreader_lexer.tokens lexer in
  let initial_pos = Mreader_lexer.initial_position lexer in
  match kind with
  | ML ->
    let result =
      parse Parser_raw.Incremental.implementation tokens initial_pos
    in
    `Implementation (Option.value result ~default:[])
  | MLI ->
    let result = parse Parser_raw.Incremental.interface tokens initial_pos in
    `Interface (Option.value result ~default:[])

let make warnings lexer kind =
  errors_ref := [];
  let tree = run_parser warnings lexer kind in
  let errors = !errors_ref in
  errors_ref := [];
  { kind; tree; errors; lexer }

let result t = t.tree

let errors t = t.errors
