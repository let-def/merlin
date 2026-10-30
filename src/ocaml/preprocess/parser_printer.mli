open Parser_raw

val print_symbol : MenhirInterpreter.xsymbol -> string
val print_value : 'a MenhirInterpreter.symbol -> 'a -> string
val print_token : token -> string

(** The symbol of a number, the terminals first (as in the tables of LRgrep) *)
val symbol_of_index : int -> MenhirInterpreter.xsymbol option
