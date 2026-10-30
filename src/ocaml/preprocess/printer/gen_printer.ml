open MenhirSdk

include Cmly_read.Read(struct let filename = Sys.argv.(1) end)

let is_attribute names attr =
  List.exists (fun l -> Attribute.has_label l attr) names

let printf = Printf.printf
let sprintf = Printf.sprintf

let menhir = "MenhirInterpreter"

(** Print header, if any *)

let print_header () =
  let name = Filename.chop_extension (Filename.basename Sys.argv.(1)) in
  printf "open %s\n" (String.capitalize_ascii name);
  List.iter
    (fun attr ->
       if is_attribute ["header"; "printer.header"] attr then
         printf "%s\n" (Attribute.payload attr))
    Grammar.attributes

(** Printer from attributes *)

let symbol_printer default attribs =
  match List.find (is_attribute ["symbol"]) attribs with
  | attr -> Attribute.payload attr
  | exception Not_found ->
    sprintf "%S" default

let print_symbol () =
  let case_t t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF ->
      printf "  | %s.X (%s.T %s.T_%s) -> %s\n"
        menhir menhir menhir
        (Terminal.name t)
        (symbol_printer (Terminal.name t) (Terminal.attributes t))
    | `PSEUDO -> ()
  and case_n n =
    match Nonterminal.kind n with
    | `REGULAR ->
      printf "  | %s.X (%s.N %s.N_%s) -> %s\n"
        menhir menhir menhir
        (Nonterminal.mangled_name n)
        (symbol_printer (Nonterminal.mangled_name n) (Nonterminal.attributes n))
    | `START -> ()
  in
  printf "let print_symbol = function\n";
  Terminal.iter case_t;
  Nonterminal.iter case_n

let value_printer default attribs =
  match List.find (is_attribute ["printer"]) attribs with
  | attr -> sprintf "(%s)" (Attribute.payload attr)
  | exception Not_found ->
    sprintf "(fun _ -> %s)" (symbol_printer default attribs)

let print_value () =
  let case_t t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF->
      printf "  | %s.T %s.T_%s -> %s\n"
        menhir menhir
        (Terminal.name t)
        (value_printer (Terminal.name t) (Terminal.attributes t))
    | `PSEUDO -> ()
  and case_n n =
    match Nonterminal.kind n with
    | `REGULAR ->
      printf "  | %s.N %s.N_%s -> %s\n"
        menhir menhir
        (Nonterminal.mangled_name n)
        (value_printer (Nonterminal.mangled_name n) (Nonterminal.attributes n))
    | `START -> ()
  in
  printf "let print_value (type a) : a %s.symbol -> a -> string = function\n"
    menhir;
  Terminal.iter case_t;
  Nonterminal.iter case_n

let print_token () =
  let case t =
    match Terminal.kind t with
    | `REGULAR | `EOF ->
      printf "  | %s%s -> print_value (%s.T %s.T_%s) %s\n"
        (Terminal.name t)
        (match Terminal.typ t with | None -> "" | Some _typ -> " v")
        menhir menhir
        (Terminal.name t)
        (match Terminal.typ t with | None -> "()" | Some _typ -> "v")
    | `PSEUDO | `ERROR -> ()
  in
  printf "let print_token = function\n";
  Terminal.iter case

(** The symbols by number, the terminals first, as in the tables of LRgrep *)

let print_symbol_of_index () =
  let case_t t =
    match Terminal.kind t with
    | `REGULAR | `ERROR | `EOF ->
      printf "  | %d -> Some (%s.X (%s.T %s.T_%s))\n"
        (Terminal.to_int t) menhir menhir menhir (Terminal.name t)
    | `PSEUDO -> ()
  and case_n n =
    match Nonterminal.kind n with
    | `REGULAR ->
      printf "  | %d -> Some (%s.X (%s.N %s.N_%s))\n"
        (Terminal.count + Nonterminal.to_int n) menhir menhir menhir
        (Nonterminal.mangled_name n)
    | `START -> ()
  in
  printf "let symbol_of_index = function\n";
  Terminal.iter case_t;
  Nonterminal.iter case_n;
  printf "  | _ -> None\n"

let () =
  print_header ();
  print_newline ();
  print_symbol ();
  print_newline ();
  print_value ();
  print_newline ();
  print_token ();
  print_newline ();
  print_symbol_of_index ()
