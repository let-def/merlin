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
    (* The text of an annotation can quote the keywords and punctuations it
       mentions too ([':' <type>]) *)
    let unquote w =
      let n = String.length w in
      if n > 2 && w.[0] = '\'' && w.[n - 1] = '\'' then String.sub w 1 (n - 2)
      else if n > 3 && w.[0] = '[' && w.[1] = '\'' && w.[n - 1] = '\'' then
        "[" ^ String.sub w 2 (n - 3)
      else w
    in
    String.concat " " (List.map unquote (String.split_on_char ' ' s))
  | C.Optional pieces ->
    let s = print pieces in
    let n = String.length s in
    if n > 1 && s.[0] = '[' && s.[n - 1] = ']' then s else "[" ^ s ^ "]"

(* The construct of a suggestion: the segments of its continuation up to
   the first one that shows something after the keyword (an operator
   continues the expression it is in), and their symbols *)
let own_segments s =
  let rec take segs acc = function
    | [] -> (List.rev segs, acc)
    | seg :: rest ->
      let acc = acc @ C.segment_display seg in
      if List.length acc > 1 then (List.rev (seg :: segs), acc)
      else take (seg :: segs) acc rest
  in
  take [] [] (C.segments s)

(* How the description of a suggestion differs from its continuation:
   the values of the annotations, of the rule of its first segment and of
   the segments of its construct *)
let description s segs =
  let rule =
    match C.segments s with
    | seg :: _ -> C.symbol_value (C.segment_rule seg)
    | [] -> None
  in
  match rule with
  | Some _ -> rule
  | None -> List.find_map C.segment_value segs

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

(* The continuation from the keyword, whose text is [first]: the optional
   pieces before it come from the other suggestions of the group (the [when]
   of a case before its [->]), and an optional piece starting with it is
   not optional, the keyword being there *)
let rec from_keyword keyword first = function
  | [] -> None
  | C.Symbol x :: rest when x = first -> Some (C.Symbol keyword :: rest)
  | C.Optional (C.Symbol x :: inner) :: rest when x = first ->
    Some ((C.Symbol keyword :: inner) @ rest)
  | C.Optional inner :: rest -> (
    match from_keyword keyword first inner with
    | Some pieces -> Some (pieces @ rest)
    | None -> from_keyword keyword first rest)
  | C.Symbol _ :: rest -> from_keyword keyword first rest

(* The continuation up to a closing delimiter of a construct opened before
   the keyword, without the separator of a list the construct is in *)
let enclosed pieces =
  let rec go depth acc = function
    | [] -> List.rev acc
    | (C.Symbol ("'('" | "'['" | "'{'") as p) :: rest ->
      go (depth + 1) (p :: acc) rest
    | C.Symbol ("')'" | "']'" | "'}'" | "'|]'") :: _ when depth = 0 ->
      List.rev acc
    | (C.Symbol ("')'" | "']'" | "'}'" | "'|]'") as p) :: rest ->
      go (depth - 1) (p :: acc) rest
    | p :: rest -> go depth (p :: acc) rest
  in
  match List.rev (go 0 [] pieces) with
  | (C.Optional [ C.Symbol ("';'" | "','") ] | C.Symbol ("[';']" | "[',']"))
    :: rest -> List.rev rest
  | pieces -> List.rev pieces

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
            let own = own_segments s and distance = C.distance suffix s in
            Hashtbl.replace group keyword
              (match Hashtbl.find_opt group keyword with
              | Some (d, own', s') ->
                if List.length (snd own') >= List.length (snd own) then
                  (min d distance, own', s')
                else (min d distance, own, s)
              | None -> (distance, own, s)))
        suggestions;
      Hashtbl.iter
        (fun keyword (distance, (segs, own), s) ->
          let key = (distance, List.length own) in
          match Hashtbl.find_opt best keyword with
          | Some (key', _) when key' <= key -> ()
          | previous ->
            if previous = None then order := keyword :: !order;
            (* The keyword is printed as itself: an operator is a class of
               tokens *)
            let pieces = cut own pieces in
            let desc =
              match description s segs with
              | Some C.Alone -> keyword
              | (Some (C.Enclosed _) | None) as description -> (
                let pieces =
                  enclosed
                    (Option.value ~default:pieces
                       (from_keyword keyword (List.hd own) pieces))
                in
                match description with
                | Some (C.Enclosed (opening, closing)) ->
                  opening ^ print pieces ^ closing
                | Some C.Alone | None -> print pieces)
            in
            Hashtbl.replace best keyword (key, desc))
        group)
    (C.complete_by_continuation env);
  let distance keyword = fst (fst (Hashtbl.find best keyword)) in
  List.rev !order
  |> List.stable_sort (fun k1 k2 -> compare (distance k1) (distance k2))
  |> List.map (fun keyword -> (keyword, snd (Hashtbl.find best keyword)))
