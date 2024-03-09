(* The beginnings of an interpreter for Kλ. *)

let char_list_of_string (s : string) : char list =
  List.init (String.length s) (String.get s)

let string_of_char_list (char_list : char list) : string =
  char_list |> List.to_seq |> String.of_seq

let fold_list_of_lists lst_of_lsts =
  List.fold_left (fun lst acc -> List.append lst acc) [] lst_of_lsts

let take_while (p : 'a -> bool) (l : 'a list) : 'a list =
  let rec aux acc p l =
    match l with
    | [] -> acc
    | lh :: lt -> if p lh then aux (lh :: acc) p lt else acc
  in
  aux [] p l |> List.rev

let rec drop_while (p : 'a -> bool) (l : 'a list) : 'a list =
  match l with [] -> [] | lh :: lt -> if p lh then drop_while p lt else l

let rec drop (n : int) (l : 'a list) : 'a list =
  if n > List.length l then []
  else match n with 0 -> l | _ -> drop (n - 1) (List.tl l)

type kl_lex =
  | LParen
  | RParen
  | Number of char list
  | String of char list
  | Symbol of char list
  | Minus
  | Dot
  | WhiteSpace of char list
  | ERROR of char list

let char_is_digit c = match c with '0' .. '9' -> true | _ -> false

let char_is_symbol_char c =
  match c with '(' | ')' | '\t' | ' ' | '"' | '\n' -> false | _ -> true

let next_lexeme current_char rest_of_chars : kl_lex =
  match current_char with
  | '(' -> LParen
  | ')' -> RParen
  | '.' -> Dot
  | '-' -> Minus
  | ' ' | '\t' | '\n' -> WhiteSpace (current_char :: [])
  | '0' .. '9' ->
      let acc_num = current_char :: take_while char_is_digit rest_of_chars in
      Number acc_num
  | '"' ->
      let acc_str = take_while (fun c -> not (c = '"')) rest_of_chars in
      String acc_str
  | _ ->
      (* TODO: add some error checking here... *)
      let acc_sym =
        current_char :: take_while char_is_symbol_char rest_of_chars
      in
      Symbol acc_sym

let rec lex_helper acc (current_char : char) (rest_of_chars : char list) =
  let next_lexeme = next_lexeme current_char rest_of_chars in
  let chars_to_drop =
    match next_lexeme with
    | Number l | Symbol l | String l -> List.length l - 1
    | _ -> 0
  in

  (* FIXME: do proper error checking here... *)
  let next_rest_of_chars =
    match next_lexeme with
    (* We add two because of the quotes for the string. *)
    | String l -> drop (chars_to_drop + 2) rest_of_chars
    | _ -> drop chars_to_drop rest_of_chars
  in

  let next_acc = next_lexeme :: acc in
  if next_rest_of_chars = [] then next_acc |> List.rev
  else
    lex_helper next_acc
      (List.hd next_rest_of_chars)
      (List.tl next_rest_of_chars)

let lex str =
  let program_char_lst = char_list_of_string str in
  lex_helper [] (List.hd program_char_lst) (List.tl program_char_lst)

(* Parser code below: *)

type kl_number = Int of int | Float of float

type kl_value =
  | Symbol of string
  | Number of kl_number
  | String of string
  | List of kl_value list

(* FIXME: I don't think this definition is *technically* correct yet. *)
type kl_expr = Value of kl_value | Expr of kl_expr

let parse (lst : kl_lex list) : kl_value =
  match lst with
  | Number int_part :: Dot :: Number dec_part :: rst ->
      Number
        (Float
           (fold_list_of_lists [ int_part; [ '.' ]; dec_part ]
           |> string_of_char_list |> float_of_string))
  | _ -> Number (Float 10.20)

(* FIXME: this is not yet correct Kλ. *)
let program = "(begin (define r 10) (* pi (* r r)) '(\"asdf\"))"
let main () = read_line () |> lex (* |> parse |> eval |> eval_print *)
let _ = main ()
