(* The beginnings of an interpreter for Kλ. *)

let explode_string (s : string) : char list =
  List.init (String.length s) (String.get s)

type kl_lex =
  | LParen
  | RParen
  | Number of char list
  | String of char list
  | Symbol of char list
  | Minus
  | Dot
  | WhiteSpace
  | ERROR of char list

let take_while (p : 'a -> bool) (l : 'a list) : 'a list =
  let rec aux acc p l =
    match l with
    | [] -> acc
    | lh :: lt -> if p lh then aux (lh :: acc) p lt else acc
  in
  aux [] p l |> List.rev

let rec drop (n : int) (l : 'a list) : 'a list =
  if n > List.length l then []
  else match n with 0 -> l | _ -> drop (n - 1) (List.tl l)

let char_is_digit c = match c with '0' .. '9' -> true | _ -> false

let char_is_symbol_char c =
  match c with '(' | ')' | '\t' | ' ' | '"' | '\n' -> false | _ -> true

let next_lexeme current_char rest_of_chars : kl_lex =
  match current_char with
  | '(' -> LParen
  | ')' -> RParen
  | '.' -> Dot
  | '-' -> Minus
  | ' ' | '\t' | '\n' -> WhiteSpace
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
  let program_char_lst = explode_string str in
  lex_helper [] (List.hd program_char_lst) (List.tl program_char_lst)

type kl_symbol = String
type kl_string = String
type kl_list = Tuple
type kl_number = Int | Float

type kl_expr =
  | Symbol of kl_symbol
  | Number of kl_number
  | String of kl_string
  | List of kl_list
  | Expr of kl_expr

(* FIXME: this is not yet correct Kλ. *)
let program = "(begin (define r 10) (* pi (* r r)) '(\"asdf\"))"
let main () =
  read_line () |> lex (* |> parse |> eval |> eval_print *)

let _ = main ()
