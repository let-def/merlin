module C = Parser_completion

(* The keywords, by terminal *)
let keyword_of_terminal =
  lazy
    (let table = Hashtbl.create 64 in
     List.iter
       (fun (text, token) ->
         Hashtbl.replace table (Parser_raw.Tables.token2terminal token) text)
       (Lexer_raw.keyword_tokens ());
     table)

(* A continuation, without the quotes of the keywords and punctuations,
   and without doubling the brackets of the optional symbols already printed
   between brackets *)
let rec print pieces = String.concat " " (List.map print_piece pieces)

and print_piece = function
  | C.Symbol s ->
    let n = String.length s in
    if n > 2 && s.[0] = '\'' && s.[n - 1] = '\'' then String.sub s 1 (n - 2)
    else s
  | C.Optional pieces ->
    let s = print pieces in
    let n = String.length s in
    if n > 1 && s.[0] = '[' && s.[n - 1] = ']' then s else "[" ^ s ^ "]"

(* The symbols of the construct of a suggestion: the segments of its
   continuation up to the first one that shows something after the keyword
   (an operator continues the expression it is in) *)
let own_symbols s =
  let rec take acc = function
    | [] -> acc
    | seg :: rest ->
      let acc = acc @ C.segment_display seg in
      if List.length acc > 1 then acc else take acc rest
  in
  take [] (C.segments s)

(* The continuation of a group of suggestions, cut after the symbols [own]
   of the construct of one of them; the optional pieces that come from the
   other suggestions are kept inside it *)
let rec cut own pieces =
  let rec symbols = function
    | C.Symbol x -> [ x ]
    | C.Optional pieces -> List.concat_map symbols pieces
  in
  let rec drop prefix l =
    match (prefix, l) with
    | [], l -> Some l
    | x :: prefix, y :: l when x = y -> drop prefix l
    | _ -> None
  in
  match (own, pieces) with
  | [], _ | _, [] -> []
  | _, (C.Symbol x as p) :: rest -> (
    match own with
    | y :: own when x = y -> p :: cut own rest
    | _ -> p :: cut own rest)
  | _, (C.Optional _ as p) :: rest -> (
    match drop (symbols p) own with
    | Some own -> p :: cut own rest
    | None -> p :: cut own rest)

(* The keywords that can come next, sorted by how soon they reconnect with
   the next tokens; each is described by the group of suggestions that
   reconnects soonest, then by the one with the shortest construct (a type
   definition rather than a type extension), with the longest construct of
   the group ([if] with its optional [else]) *)
let keywords (Mreader_parser.Snapshot (env, suffix)) =
  let table = Lazy.force keyword_of_terminal in
  let suffix = List.map Parser_raw.Tables.token2terminal suffix in
  let best = Hashtbl.create 16 and order = ref [] in
  List.iter
    (fun (pieces, suggestions) ->
      (* The suggestions of the group, by keyword *)
      let group = Hashtbl.create 7 in
      List.iter
        (fun s ->
          match Hashtbl.find_opt table s.C.terminal with
          | None -> ()
          | Some keyword ->
            let own = own_symbols s and distance = C.distance suffix s in
            Hashtbl.replace group keyword
              (match Hashtbl.find_opt group keyword with
              | Some (d, own') ->
                ( min d distance,
                  if List.length own' >= List.length own then own' else own )
              | None -> (distance, own)))
        suggestions;
      Hashtbl.iter
        (fun keyword (distance, own) ->
          let key = (distance, List.length own) in
          match Hashtbl.find_opt best keyword with
          | Some (key', _) when key' <= key -> ()
          | previous ->
            if previous = None then order := keyword :: !order;
            (* The keyword is printed as itself: an operator is a class of
               tokens *)
            let desc =
              match cut own pieces with
              | C.Symbol _ :: rest -> print (C.Symbol keyword :: rest)
              | pieces -> print pieces
            in
            Hashtbl.replace best keyword (key, desc))
        group)
    (C.complete_by_continuation env);
  let distance keyword = fst (fst (Hashtbl.find best keyword)) in
  List.rev !order
  |> List.stable_sort (fun k1 k2 -> compare (distance k1) (distance k2))
  |> List.map (fun keyword -> (keyword, snd (Hashtbl.find best keyword)))
