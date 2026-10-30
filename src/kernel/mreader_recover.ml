(* The tokens of OCaml for the lexical bracket policy of the recovery of
   LRgrep ({!Lrgrep_recovery.lexical}): the delimiters, the starts of the
   structure items and the local bindings, which do not depend on the
   layout.

   The parser uses the indentation policy (see {!Mreader_parser}), the
   closest to the former recovery of Merlin. This policy is an alternative,
   for the inputs whose layout cannot be trusted:
   [R.parse (R.lexical Mreader_recover.lexical) ...]. *)

open Parser_raw

(* The delimiters: ( ), begin/struct/sig/object ... end, do ... done,
   [ ] and its variants, [| |], { }, {< >}. A keyword after [%] or an
   attribute or extension bracket names the attribute or the extension:
   it is not a delimiter. *)
let delimiters prev tok =
  match prev with
  | Some
      ( PERCENT
      | LBRACKETPERCENT
      | LBRACKETPERCENTPERCENT
      | LBRACKETAT
      | LBRACKETATAT
      | LBRACKETATATAT ) -> []
  | _ -> (
    match tok with
    | LPAREN -> [ `Open 0 ]
    | BEGIN | STRUCT | SIG | OBJECT -> [ `Open 1 ]
    | DO -> [ `Open 2 ]
    | LBRACKET
    | LBRACKETLESS
    | LBRACKETGREATER
    | LBRACKETPERCENT
    | LBRACKETPERCENTPERCENT
    | LBRACKETAT
    | LBRACKETATAT
    | LBRACKETATATAT -> [ `Open 3 ]
    | LBRACKETBAR -> [ `Open 4 ]
    | LBRACE -> [ `Open 5 ]
    | LBRACELESS -> [ `Open 6 ]
    | RPAREN -> [ `Close 0 ]
    | END -> [ `Close 1 ]
    | DONE -> [ `Close 2 ]
    | RBRACKET -> [ `Close 3 ]
    | BARRBRACKET -> [ `Close 4 ]
    | RBRACE -> [ `Close 5 ]
    | GREATERRBRACE -> [ `Close 6 ]
    | _ -> [])

(* The keywords that start a structure item *)
let item_keyword = function
  | LET
  | TYPE
  | MODULE
  | OPEN
  | INCLUDE
  | EXCEPTION
  | EXTERNAL
  | CLASS
  | VAL
  | LBRACKETATATAT -> true
  | _ -> false

(* The tokens after which an expression can start *)
let before_expression = function
  | EQUAL
  | IN
  | MINUSGREATER
  | LPAREN
  | SEMI
  | THEN
  | ELSE
  | DO
  | BEGIN
  | COMMA
  | COLONCOLON
  | COLONEQUAL
  | LESSMINUS
  | LBRACKET
  | LBRACKETBAR
  | WHEN
  | TRY
  | MATCH
  | IF
  | WHILE
  | TO
  | DOWNTO
  | LAZY
  | ASSERT
  | BARBAR
  | AMPERAMPER
  | AMPERSAND
  | OR
  | PLUS
  | MINUS
  | STAR
  | PLUSDOT
  | MINUSDOT
  | LESS
  | GREATER
  | BANG
  | INFIXOP0 _
  | INFIXOP1 _
  | INFIXOP2 _
  | INFIXOP3 _
  | INFIXOP4 _
  | PREFIXOP _
  | HASHOP _
  | DOTOP _
  | LABEL _
  | OPTLABEL _ -> true
  | _ -> false

(* Whether an item keyword can follow [prev] inside an item *)
let continues_into prev = function
  | LET -> before_expression prev
  | TYPE -> (
    match prev with
    | LPAREN | WITH | AND | MODULE | CLASS | COLON -> true
    | _ -> false)
  | MODULE -> (
    match prev with
    | LET | LPAREN | WITH | AND | INCLUDE | COLON | EQUAL -> true
    | _ -> false)
  | OPEN -> prev = LET
  | EXCEPTION -> (
    match prev with
    | LET | BAR | WITH | LPAREN -> true
    | _ -> false)
  | VAL -> prev = LPAREN
  | _ -> false

(* An item keyword starts an item unless the token before it can be
   followed by it inside an item: unlike a token that can end an
   expression, this holds after the token left at the end of an
   unfinished definition ([let let], [| let]...). [;;] is an item of its
   own (a frame of the stack, after the item it ends), and the token after
   it starts an item. *)
let starts_item prev tok =
  (item_keyword tok && not (continues_into prev tok))
  || (prev = SEMISEMI && tok <> EOF)
  || tok = SEMISEMI

let binding = function
  | LET | LETOP _ -> `Let
  | IN -> `In
  | EQUAL -> `Equal
  | _ -> `Other

let lexical : token Lrgrep_recovery.lexical =
  { Lrgrep_recovery.delimiters; starts_item; binding }
