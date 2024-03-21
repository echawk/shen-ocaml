(* The beginnings of an interpreter for Kλ. *)

module SizedList = struct
  type 'a sized_list = { lst : 'a list; size : int }

  let init (l : 'a list) : 'a sized_list = { lst = l; size = List.length l }
  let length (l : 'a sized_list) : int = l.size

  let cons (e : 'a) (l : 'a sized_list) : 'a sized_list =
    { lst = e :: l.lst; size = l.size + 1 }

  let hd (l : 'a sized_list) : 'a = List.hd l.lst

  let tl (l : 'a sized_list) : 'a sized_list =
    { lst = List.tl l.lst; size = l.size - 1 }

  let rev (l : 'a sized_list) : 'a sized_list =
    { lst = List.rev l.lst; size = l.size }

  let rec drop (n : int) (l : 'a sized_list) : 'a sized_list =
    let rec aux n l s =
      if n > s then { lst = []; size = 0 }
      else
        match n with
        | 0 -> { lst = l; size = s }
        | _ -> aux (n - 1) (List.tl l) (s - 1)
    in
    aux n l.lst l.size

  let drop_while (p : 'a -> bool) (l : 'a sized_list) : 'a sized_list =
    let rec aux p l s =
      match l with
      | [] -> { lst = []; size = 0 }
      | lh :: lt -> if p lh then aux p lt (s - 1) else { lst = l; size = s }
    in
    aux p l.lst l.size

  let take_while (p : 'a -> bool) (l : 'a sized_list) : 'a sized_list =
    let rec aux acc p l s =
      match l with
      | [] -> { lst = acc; size = s }
      | lh :: lt ->
          if p lh then aux (lh :: acc) p lt (s + 1) else { lst = acc; size = s }
    in
    aux [] p l.lst 0 |> rev

  let to_list (l : 'a sized_list) : 'a list = l.lst
end

(* Defining some missing utility functions. *)
let char_list_of_string (s : string) : char list =
  List.init (String.length s) (String.get s)

let string_of_char_list (char_list : char list) : string =
  char_list |> List.to_seq |> String.of_seq

let join_list_of_lists lst_of_lsts =
  List.fold_left (fun lst acc -> List.append lst acc) [] lst_of_lsts

let read_string_from_file filepath =
  In_channel.with_open_text filepath In_channel.input_all

(*
  Simple little timing function (thanks stackoverflow!)
  Source: https://web.archive.org/web/20140720233328/https://stackoverflow.com/questions/9061421/running-time-in-ocaml
 *)

let time f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  fx

type kl_lex =
  | LParen
  | RParen
  | Number of char list
  | String of char list
  | Symbol of char list
  | WhiteSpace
  | ERROR of char list

let char_is_digit c = match c with '0' .. '9' -> true | _ -> false

let char_is_symbol_char c =
  match c with '(' | ')' | '\t' | ' ' | '"' | '\n' -> false | _ -> true

let lex_number_helper (hd : char) (tl : char SizedList.sized_list) : kl_lex =
  let acc_num =
    SizedList.cons hd (SizedList.take_while char_is_digit tl)
    |> SizedList.to_list
  in
  let potential_tl = SizedList.drop (List.length acc_num - 1) tl in
  match potential_tl.lst with
  | '.' :: '0' .. '9' :: _ ->
      let acc_num_two =
        SizedList.take_while char_is_digit (SizedList.tl potential_tl)
        |> SizedList.to_list
      in
      Number (join_list_of_lists [ acc_num; [ '.' ]; acc_num_two ])
  | _ -> Number acc_num

let lex_number (hd : char) (tl : 'a SizedList.sized_list) : kl_lex =
  match hd with
  | '0' .. '9' -> lex_number_helper hd tl
  | '-' -> (
      match tl.lst with
      | '0' .. '9' :: _ -> lex_number_helper hd tl
      | _ -> Symbol [ hd ])
  | _ -> ERROR [ hd ]

let next_lexeme current_char (rest_of_chars : 'a SizedList.sized_list) : kl_lex
    =
  match current_char with
  | '(' -> LParen
  | ')' -> RParen
  | ' ' | '\t' | '\n' -> WhiteSpace
  | '-' | '0' .. '9' -> lex_number current_char rest_of_chars
  | '"' ->
      let acc_str =
        SizedList.take_while (fun c -> not (c = '"')) rest_of_chars
        |> SizedList.to_list
      in
      String acc_str
  | _ ->
      (* TODO: add some error checking here... *)
      let acc_sym =
        current_char
        :: (SizedList.take_while char_is_symbol_char rest_of_chars
           |> SizedList.to_list)
      in
      Symbol acc_sym

let rec lex_helper acc (current_char : char)
    (rest_of_chars : char SizedList.sized_list) =
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
    | String l -> SizedList.drop (chars_to_drop + 2) rest_of_chars
    | _ -> SizedList.drop chars_to_drop rest_of_chars
  in

  let next_acc = next_lexeme :: acc in
  if next_rest_of_chars.lst = [] then next_acc |> List.rev
  else
    lex_helper next_acc
      (SizedList.hd next_rest_of_chars)
      (SizedList.tl next_rest_of_chars)

let lex str =
  let program_char_lst = char_list_of_string str |> SizedList.init in
  lex_helper [] (SizedList.hd program_char_lst) (SizedList.tl program_char_lst)
  |> List.filter (fun lexeme ->
         match lexeme with WhiteSpace -> false | _ -> true)

(* Parser code below: *)

type kl_number = Int of int | Float of float

type kl_value =
  | ERROR of kl_lex
  | Symbol of string
  | Number of kl_number
  | String of string
  | List of kl_value list

(* FIXME: I don't think this definition is *technically* correct yet. *)
type kl_expr = Value of kl_value | Expr of kl_expr

let parse_number (lst : kl_lex list) : kl_value * kl_lex list =
  match lst with
  | Number lst :: rst ->
      ( Number
          (let lst_str = string_of_char_list lst in
           if List.mem '.' lst then Float (float_of_string lst_str)
           else Int (int_of_string lst_str)),
        rst )
  | _ -> (ERROR (List.hd lst), List.tl lst)

let parse_symbol (lst : kl_lex list) : kl_value * kl_lex list =
  match lst with
  | Symbol char_lst :: rst -> (Symbol (char_lst |> string_of_char_list), rst)
  | _ -> (ERROR (List.hd lst), List.tl lst)

let parse_string (lst : kl_lex list) : kl_value * kl_lex list =
  match lst with
  | String char_lst :: rst -> (String (char_lst |> string_of_char_list), rst)
  | _ -> (ERROR (List.hd lst), List.tl lst)

let parse_atom (lst : kl_lex list) : kl_value * kl_lex list =
  match List.hd lst with
  | String _ -> parse_string lst
  | Symbol _ -> parse_symbol lst
  | Number _ -> parse_number lst
  | _ -> (ERROR (List.hd lst), List.tl lst)

let rec parse_helper (acc : kl_value list) (lst : kl_lex list) : kl_value list =
  let parse_result, rst =
    match lst with
    | String _ :: _ | Symbol _ :: _ | Number _ :: _ -> parse_atom lst
    | _ -> (ERROR (List.hd lst), List.tl lst)
  in
  let next_acc = parse_result :: acc in
  match rst with [] -> next_acc | _ -> parse_helper next_acc rst

let parse (lst : kl_lex list) = parse_helper [] lst |> List.rev


(* FIXME: this is not yet correct Kλ. *)
let program = "(begin (define r 10) (* pi (* r r)) '(\"asdf\"))"

let main () =
  (if Array.length Sys.argv > 1 then time read_string_from_file Sys.argv.(1)
   else time read_line ())
  |> time lex |> time parse

(* |> parse |> eval |> eval_print *)
(* let _ = main () *)
