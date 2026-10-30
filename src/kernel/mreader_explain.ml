open Parser_raw
open MenhirInterpreter

let opening (type a) : a terminal -> string option = function
  | T_STRUCT -> Some "struct"
  | T_SIG -> Some "sig"
  | T_OBJECT -> Some "object"
  | T_BEGIN -> Some "begin"
  | T_LPAREN -> Some "("
  | T_LBRACKET -> Some "["
  | T_LBRACE -> Some "{"
  | T_LBRACKETBAR -> Some "[|"
  | T_LBRACKETLESS -> Some "[<"
  | T_LBRACELESS -> Some "{<"
  | _ -> None

let opening_st st =
  match incoming_symbol st with
  | T term -> opening term
  | _ -> None

let closing (type a) : a terminal -> bool = function
  | T_END -> true
  | T_RPAREN -> true
  | T_RBRACKET -> true
  | T_RBRACE -> true
  | T_BARRBRACKET -> true
  | T_GREATERRBRACE -> true
  | T_GREATERRBRACKET -> true
  | _ -> false

let closing_st st =
  match incoming_symbol st with
  | T term -> closing term
  | _ -> false

type explanation =
  { unclosed : (string * Location.t) option;
    location : Location.t;
    shifted : MenhirInterpreter.xsymbol option;
    unexpected : MenhirInterpreter.token
  }

(* [shifted] is the first symbol the recovery would shift *)
let explain env (unexpected, startp, endp) shifted =
  let mkloc s e = { Location.loc_start = s; loc_end = e; loc_ghost = false } in
  let open MenhirInterpreter in
  let location = mkloc startp endp in
  let closed = ref 0 in
  let unclosed = ref None in
  let rec process env =
    match top env with
    | None -> ()
    | Some (Element (st, _, startp, endp)) -> (
      if closing_st st then incr closed;
      begin match opening_st st with
      | None -> ()
      | Some st ->
        if !closed = 0 && !unclosed = None then
          unclosed := Some (st, mkloc startp endp)
        else decr closed
      end;
      match pop env with
      | None -> ()
      | Some env -> process env)
  in
  process env;
  { unclosed = !unclosed; location; shifted; unexpected }

let to_error { unclosed; location; shifted; unexpected = _ } =
  let after =
    match unclosed with
    | None -> ""
    | Some (name, _) -> " after unclosed " ^ name
  in
  let friendly_name sym =
    match sym with
    | X (T _) -> "`" ^ Parser_printer.print_symbol sym ^ "'"
    | X (N _) -> Parser_printer.print_symbol sym
  in
  let expecting =
    match shifted with
    | None | Some (X (T T_EOF)) -> ""
    | Some sym -> ", expecting " ^ friendly_name sym
  in
  let msg = Printf.sprintf "Syntax error%s%s" after expecting in
  Location.error ~loc:location ~source:Location.Parser msg

exception Syntax_explanation of explanation

let syntax_explanation = function
  | Syntax_explanation explanation -> Some (to_error explanation)
  | _ -> None

let () = Location.register_error_of_exn syntax_explanation
