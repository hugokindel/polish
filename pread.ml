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
