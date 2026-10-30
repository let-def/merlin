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

type kind = ML | MLI
(*| MLL | MLY*)

type t

val make :
  ?for_completion:Lexing.position ->
  Warnings.state ->
  Mreader_lexer.t ->
  kind ->
  t

type tree =
  [ `Interface of Parsetree.signature | `Implementation of Parsetree.structure ]

val result : t -> tree

val errors : t -> exn list

(** The stack of the parser before the token at the cursor of a completion
    (the identifier or keyword being typed, or the empty identifier inserted
    at the cursor), and the next tokens of the input after this one (20 at
    most). *)
type snapshot =
  | Snapshot :
      'a Parser_raw.MenhirInterpreter.env * Parser_raw.token list
      -> snapshot

(** The stack at the cursor, when parsing for a completion *)
val snapshot : t -> snapshot option
