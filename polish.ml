open Printf
open Ptypes
open Pread
open Pprint
open Peval
open Psimpl
open Pvars
open Putils
open Psign



(** Reads a Polish program from a filename. *)
let read_polish (filename: string): program =
  let lines = read_file filename in
  parse_block lines

(** Prints a parsed Polish program. *)
let print_polish (program: program): unit =
  print_block program

(** Evaluate a parsed Polish program. *)
let eval_polish (program:  program): unit =
  let _ = eval_block program in ()

(** Prints a simplified and parsed Polish program. *)
let simpl_polish (program: program): program =
  simpl_block program

(** Prints a list of initialized and uninitialized variables of a parsed Polish program. *)
let vars_polish (program: program): unit =
  let (vars_init, vars_uninit) = vars_block program in
  print_vars vars_init vars_uninit

(** TODO: Print possible signs for variables of a parsed Polish program *)
let sign_polish (program: program): unit =
  let (env,error) = sign_block program in
  print_sign env error

(** Usage of the CLI. *)
let usage () =
  printf "polish: static analysis of a mini-language\n";
  printf "\n";
  printf "usage:\n";
  printf "\tpolish -reprint FILENAME -> parses and reprints a polish program\n";
  printf "\tpolish -eval FILENAME -> parses and evaluates a polish program\n";
  printf "\tpolish -simpl FILENAME -> parses, simplifies and reprints a polish program\n";
  printf "\tpolish -vars FILENAME -> parses and prints variables non initialize before access of a polish program\n";
  printf "\tpolish -sign FILENAME -> parses and prints variables of a polish program with their probable signs\n"

(** Main function *)
let main () =
  try
    match Sys.argv with
    | [|_;"-reprint";file|] -> print_polish (read_polish file)
    | [|_;"-eval";file|] -> eval_polish (simpl_polish (read_polish file))
    | [|_;"-simpl";file|] -> print_polish (simpl_polish (read_polish file))
    | [|_;"-vars";file|] -> vars_polish (read_polish file)
    | [|_;"-sign";file|] -> sign_polish (read_polish file)
    | _ -> usage ()
  with Failure message ->
    printf "%s\n" message;
    exit 1

(** Calls the main function. *)
let () = main ()