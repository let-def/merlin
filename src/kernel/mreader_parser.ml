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

(* The message of the specification of the syntax errors
   (parse_errors.lrgrep), and a generic explanation when it has none *)
let on_error env ((_, loc_start, loc_end) as token) =
  let error =
    match Parse_errors.error_messages env token with
    | Some (msg, sub) ->
      let loc = { Location.loc_start; loc_end; loc_ghost = false } in
      let sub = List.map (fun (loc, msg) -> Location.msg ~loc "%s" msg) sub in
      Location.Error (Location.error ~loc ~sub ~source:Location.Parser msg)
    | None | (exception _) ->
      Mreader_explain.Syntax_explanation
        (Mreader_explain.explain env token (shifted env))
  in
  errors_ref := error :: !errors_ref

(* The recovery needs the end of input *)
let with_eof initial_pos tokens =
  match List.rev tokens with
  | (Parser_raw.EOF, _, _) :: _ -> tokens
  | [] -> [ (Parser_raw.EOF, initial_pos, initial_pos) ]
  | (_, _, endp) :: _ -> tokens @ [ (Parser_raw.EOF, endp, endp) ]

module I = Parser_raw.MenhirInterpreter

(* The stack of the parser before the token at the cursor of a completion,
   and the tokens after this one *)
type snapshot = Snapshot : 'a I.env * Parser_raw.token list -> snapshot

(* The token at the cursor [pos]: the first one ending after it, or ending
   at it if it is an identifier or a keyword being typed (the lexer of the
   completion inserted an empty identifier at the cursor otherwise) *)
let cursor_index tokens pos =
  let n = Array.length tokens in
  let rec find i =
    if i >= n then n - 1
    else
      let tok, _, (endp : Lexing.position) = tokens.(i) in
      let c = compare endp.pos_cnum pos.Lexing.pos_cnum in
      if c > 0 then i
      else if c = 0 then
        match tok with
        | Parser_raw.LIDENT _ | Parser_raw.UIDENT _ -> i
        | tok when Lexer_raw.as_keyword tok <> None -> i
        | _ -> find (i + 1)
      else find (i + 1)
  in
  find 0

(* Run the parser until it needs a token, accepts or fails *)
let rec run : 'a I.checkpoint -> 'a I.checkpoint = function
  | (I.Shifting _ | I.AboutToReduce _) as cp -> run (I.resume cp)
  | cp -> cp

(* The parser, driving the recovery, guided by the positions of the frames
   of the stack: a line less indented than a construct should not continue
   it. With a cursor, the stack before the token at the cursor is recorded,
   and the recovery does not cut the stack around it: the completion needs
   the constructs open at the cursor. *)
let parse ?cursor entry tokens initial_pos =
  let tokens = Array.of_list (with_eof initial_pos tokens) in
  let n = Array.length tokens in
  let guide = R.frames () in
  let cursor = Option.map (cursor_index tokens) cursor in
  let snapshot = ref None in
  let record env i =
    match cursor with
    | Some c when i = c && Option.is_none !snapshot ->
      let suffix =
        Array.to_list (Array.sub tokens (i + 1) (max 0 (min 20 (n - i - 1))))
      in
      snapshot :=
        Some (Snapshot (env, List.map (fun (tok, _, _) -> tok) suffix))
    | _ -> ()
  in
  let last_error = ref (-1) and last_repair = ref (-1) in
  (* The first token, before [i], starting at or after [p] *)
  let index_at (p : Lexing.position) i =
    let lo = ref 0 and hi = ref i in
    while !lo < !hi do
      let mid = (!lo + !hi) / 2 in
      let _, (s : Lexing.position), _ = tokens.(mid) in
      if s.pos_cnum < p.pos_cnum then lo := mid + 1 else hi := mid
    done;
    !lo
  in
  let rec normal env i =
    record env i;
    if i >= n then recover env i
    else
      match run (I.offer (I.input_needed env) tokens.(i)) with
      | I.InputNeeded env' -> normal env' (i + 1)
      | I.Accepted v -> Some v
      | I.HandlingError _ | I.Rejected -> error env i None
      | I.Shifting _ | I.AboutToReduce _ -> assert false
      | exception exn -> error env i (Some exn)
  and error env i exn =
    (* A cut resumes from an earlier token, and can meet the same error
       again: it is reported once *)
    if i > !last_error then begin
      last_error := i;
      match exn with
      | Some exn -> Msupport.raise_error exn
      | None -> on_error env tokens.(i)
    end;
    recover env i
  and recover env i =
    let env, i =
      match guide.R.cut env with
      | Some (env', p) ->
        let k = index_at p i in
        let around_cursor =
          match cursor with
          | Some c -> k <= c && c <= i
          | None -> false
        in
        if k > !last_repair && not around_cursor then begin
          last_repair := k;
          (env', k)
        end
        else (env, i)
      | None -> (env, i)
    in
    let r = R.recover (guide.R.start env) env in
    let rec loop j =
      (* The cursor is in the tokens of the recovery: the stack before it is
         the one of the recovery *)
      record env j;
      if j >= n then R.finish r
      else
        match R.feed r tokens.(j) with
        | `Recovering -> loop (j + 1)
        | `Resume env -> normal env (j + 1)
        | `Accept v -> Some v
    in
    loop i
  in
  let result =
    match run (entry initial_pos) with
    | I.InputNeeded env -> normal env 0
    | _ -> None
  in
  (result, !snapshot)

type t =
  { kind : kind;
    tree : tree;
    errors : exn list;
    lexer : Mreader_lexer.t;
    snapshot : snapshot option
  }

let run_parser ?for_completion warnings lexer kind =
  let snapshot = ref None in
  let tree =
    Msupport.catch_errors warnings errors_ref @@ fun () ->
    let tokens = Mreader_lexer.tokens lexer in
    let initial_pos = Mreader_lexer.initial_position lexer in
    let parse entry =
      let result, s = parse ?cursor:for_completion entry tokens initial_pos in
      snapshot := s;
      result
    in
    match kind with
    | ML ->
      `Implementation
        (Option.value (parse Parser_raw.Incremental.implementation) ~default:[])
    | MLI ->
      `Interface
        (Option.value (parse Parser_raw.Incremental.interface) ~default:[])
  in
  (tree, !snapshot)

let make ?for_completion warnings lexer kind =
  errors_ref := [];
  let tree, snapshot = run_parser ?for_completion warnings lexer kind in
  let errors = !errors_ref in
  errors_ref := [];
  { kind; tree; errors; lexer; snapshot }

let result t = t.tree

let errors t = t.errors

let snapshot t = t.snapshot
