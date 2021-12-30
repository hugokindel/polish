open Str
open Ptypes

(** Returns the indentation of line (a list of words). *)
let get_indentation (words: string list): int =

  let rec get_indentation_rec words acc =
    match words with
    | [] -> acc / 2
    | word::words' ->
      match word with
      | "" -> get_indentation_rec words' (acc + 1)
      | _ -> acc / 2 in

  get_indentation_rec words 0

(** Returns a line (a list of words) without empty words. *)
let clean_words (words: string list): string list =
  let rec clean_words_rec words acc =
    match words with
    | [] -> acc
    | word::words' ->
      match word with
      | "" -> clean_words_rec words' acc
      | _ -> clean_words_rec words' (acc @ [word]) in
  clean_words_rec words []

(** Reads a Polish file and returns a list of lines (without comment lines and without empty words). *)
let read_file (filename: string): line list =

  let rec read_file_rec (ic: in_channel) (lines_acc: line list) (position_acc: int) =
    try
      (* Split a line into words. *)
      let words = String.split_on_char ' ' (input_line ic) in
      let indent = get_indentation words in
      let words = clean_words words in
      let pos = position_acc + 1 in
      if List.length words = 0 then
        (* If a line is empty, ignore it. *)
        read_file_rec ic lines_acc pos
      else
        if String.equal (List.hd words) "COMMENT" then
          (* If a line start with `COMMENT`, ignore it. *)
          read_file_rec ic lines_acc pos
        else
          (* Else, add it to the list of lines. *)
          let line = ((pos, indent, words): line) in
          read_file_rec ic (lines_acc @ [line]) pos
    with e ->
      if e = End_of_file then
        (close_in ic; lines_acc)
      else
        (close_in_noerr ic; failwith "polish: cannot read file") in

  let ic = open_in filename in
  read_file_rec ic [] 0

(** Parses an expression. *)
let rec parse_expr (words: string list): expr * string list =
  match words with
  | [] -> failwith "polish: cannot parse expression: `words` is empty"
  | word::words' ->

    (* Check if a given word is an integer (by comparing it with a regex). *)
    let is_integer word =
      Str.string_match (Str.regexp "^[+-]?[0-9]+$") word 0 in

    (* Check if a given word is an arithmetic operator. *)
    let is_op word =
      String.equal word "+" || String.equal word "-" || String.equal word "*" ||
      String.equal word "/" || String.equal word "%" in

    if is_integer word then
      ((Num (int_of_string word)), words')
    else if (is_op word) then
      (* Parse the first expression of the arithmetic expression. *)
      let (expr1, next_words) = parse_expr words' in
      (* Parse the second expression of the arithmetic expression. *)
      let (expr2, next_words') = parse_expr next_words in
      (* Parse the  arithmetic operator. *)
      match word with
      | "+" -> (Op (Add, expr1, expr2)), next_words'
      | "-" -> (Op (Sub, expr1, expr2)), next_words'
      | "*" -> (Op (Mul, expr1, expr2)), next_words'
      | "/" -> (Op (Div, expr1, expr2)), next_words'
      | "%" -> (Op (Mod, expr1, expr2)), next_words'
      | _ -> failwith "polish: cannot parse `%` expression: unknown arithmetic operator"
    else
      ((Var (word: name)), words')

(** Parses a condition. *)
let parse_cond (words: string list): cond * string list =
  match words with
  | [] -> failwith "polish: cannot parse condition: `words` is empty"
  | _ ->
    (* Parse the expression at the left of a condition. *)
    let (left_expr, next_words) = parse_expr words in
    match next_words with
    | [] -> failwith "polish: cannot parse condition: `next_words` is empty"
    | word::words' ->
      (* Parse the expression at the right of a condition. *)
      let (right_expr, next_words') = parse_expr words' in
      (* Parse the comparison operator. *)
      match word with
      | "=" -> (left_expr, Eq, right_expr), next_words'
      | "<>" -> (left_expr, Ne, right_expr), next_words'
      | "<"-> (left_expr, Lt, right_expr), next_words'
      | "<=" -> (left_expr, Le, right_expr), next_words'
      | ">" -> (left_expr, Gt, right_expr), next_words'
      | ">=" -> (left_expr, Ge, right_expr), next_words'
      | _ -> failwith "polish: cannot parse condition: unknown comparison operator"

(** Parses a code block. *)
let parse_block (lines: line list): block =

  let rec parse_block_rec lines block_indent acc =

    let parse_set pos indent words lines =
      match words with
      | [] -> failwith "polish: cannot parse block: instruction missing"
      | name::words ->
        match words with
        | [] -> failwith "polish: cannot parse block: unknown instruction"
        | op::words' ->
          if String.equal op ":=" then
            (* If the operator `:=` is found we are parsing a `SET`. *)
            let (expr, _) = parse_expr words' in
            let instr = Set (name, expr) in
            parse_block_rec lines indent (acc @ [pos, instr])
          else
            (* We are parsing something unexpected. *)
            failwith "polish: cannot parse block: unknown instruction" in

    let parse_print pos indent words lines =
      let (expr, _) = parse_expr words in
      let instr = Print expr in
      parse_block_rec lines indent (acc @ [pos, instr]) in

    let parse_read pos indent words lines =
      let instr = Read (List.hd words) in
      parse_block_rec lines indent (acc @ [pos, instr]) in

    let parse_if pos indent words lines =
      let (if_block, next_words) = parse_block_rec  lines (indent + 1) [] in
      let (cond, _) = parse_cond words in
      (match next_words with
      | [] ->
        (* If there is no instructions after the `IF`, parse it and continue parsing. *)
        let instr = If (cond, if_block, []) in
        parse_block_rec next_words indent (acc @ [pos, instr])
      | next_line::next_lines ->
        match next_line with
        | (_, _, words') ->
          match List.hd words' with
          | "ELSE" ->
            (* If there is an `ELSE` block, parse its block too and continue parsing. *)
            let (else_block, next_lines') = parse_block_rec next_lines (indent + 1) [] in
            let instr = If (cond, if_block, else_block) in
            parse_block_rec next_lines' indent (acc @ [pos, instr])
          | _ ->
            (* If there are others instructions after the `IF`, parse it and continue parsing. *)
            let instr = If (cond, if_block, []) in
            parse_block_rec next_words indent (acc @ [pos, instr])) in

    let parse_while pos indent words lines =
      let (while_block, next_words) = parse_block_rec  lines (indent + 1) [] in
      let (cond, _) = parse_cond words in
      let instr = While (cond, while_block) in
      parse_block_rec next_words indent (acc @ [pos, instr]) in

    match lines with
    | [] -> (acc, lines)
    | line::lines' ->
      match line with
      | (pos, line_indent, words) ->
        if line_indent < block_indent then (acc, lines)
        else
          match List.hd words with
          | "PRINT" -> parse_print pos line_indent (List.tl words) lines'
          | "READ" -> parse_read pos line_indent (List.tl words) lines'
          | "IF" -> parse_if pos line_indent (List.tl words) lines'
          | "WHILE" -> parse_while pos line_indent (List.tl words) lines'
          | _ -> parse_set pos line_indent words lines' in

  let (block, _) = parse_block_rec lines 0 [] in
  block