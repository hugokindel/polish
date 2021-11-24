open Printf
open Ptypes
open Pread
open Pprint

(** Reads a Polish program from a filename. *)
let read_polish (filename: string): program =
  let lines = read_file filename in
  parse_block lines

(** Prints a parsed Polish program. *)
let print_polish (program: program): unit =
  print_block program

(** TODO: Evaluates a Polish program. *)
let eval_polish (program:  program): unit =
  failwith "TODO"

(** Usage of the CLI. *)
let usage () =
  printf "polish: static analysis of a mini-language\n";
  printf "\n";
  printf "usage:\n";
  printf "\tpolish -reprint FILENAME -> parses and reprints a polish program\n";
  printf "\tpolish -eval FILENAME -> parses and evaluates a polish program\n"

(** Main function *)
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(** Calls the main function. *)
let () = main ()