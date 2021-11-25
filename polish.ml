open Printf
open Ptypes
open Pread
open Pprint
open Psimpl
open Pvar
open Peval

(** Reads a Polish program from a filename. *)
let read_polish (filename: string): program =
  let lines = read_file filename in
  parse_block lines

(** Prints a parsed Polish program. *)
let print_polish (program: program): unit =
  print_block program

(** Evaluate a parsed Polish program. *)
let eval_polish (program:  program): unit =
  let env = eval_block program in ()

(** Prints a simplified and parsed Polish program. *)
let simpl_polish (program: program): program =
  (propa_block program: program)

(** Prints all variables and variables non initialize before acces of a parsed Polish program. *)
let var_non_init_polish (program: program): unit =
	let (vEtu,vNonInit) = search_block program in
	print_vars vEtu vNonInit

(** TODO: Print possible signs for variables of a Polish program *)
let sign_polish (program: program): unit =
  failwith "TODO"

(** Usage of the CLI. *)
let usage () =
  printf "polish: static analysis of a mini-language\n";
  printf "\n";
  printf "usage:\n";
  printf "\tpolish -reprint FILENAME -> parses and reprints a polish program\n";
  printf "\tpolish -eval FILENAME -> parses and evaluates a polish program\n";
  printf "\tpolish -simpl FILENAME -> parses, simplifies and reprints a polish program\n";
  printf "\tpolish -vars FILENAME -> parses and prints variables non initialize before acces of a polish program\n"

(** Main function *)
let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish (simpl_polish (read_polish file))
  | [|_;"-vars";file|] -> var_non_init_polish (read_polish file)
  | [|_;"-sign";file|] -> sign_polish (read_polish file)
  | _ -> usage ()

(** Calls the main function. *)
let () = main ()