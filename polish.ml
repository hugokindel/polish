open Printf
open Ptypes
open Pread
open Pprint
open Peval
open Psimpl
open Pvars


open Putils

module SymbolTable = Map.Make(String)

let sign_of_int int: sign list =
  if int = 0 then
    [Zero]
  else
    if int < 0 then
      [Neg]
    else
      [Pos]

let rec join_sign l1 l2: sign list =
    match l2 with
    | [] -> l1
    | x::t -> if List.mem x l1 then join_sign l1 t else join_sign (l1 @ [x]) t

let rec equal_sign l1 l2: bool =
    match l1 with
    | [] -> true
    | x::t -> if List.mem x l2 then equal_sign t l2 else false

let join_env env1 env2: sign list SymbolTable.t =
  SymbolTable.union (fun x y z -> Some (join_sign y z)) env1 env2

let actualize error pos: int =
  if error != 0 then error else pos

let getFirstError errorIF errorELSE: int =
  if errorIF != 0 then errorIF else errorELSE

let equal_env env1 env2: bool =
  SymbolTable.equal (fun x y -> equal_sign x y) env1 env2


let sign_op (op: op) lst1 lst2 error pos: sign list * int =
  let rec sign_op_rec op lst1 lst2 error pos acc = match lst1 with
  | [] -> acc
  | x::t ->
    let rec verif_rec op sg1 lst2 error pos acc=
    match list2 with
    | [] -> acc
    | y::l ->
      (match op, sg1, y with
      | Add, Neg, Neg -> verif_rec op sg1 l error pos (join_sign acc [Neg])
      | Div, _, Zero -> verif_rec op sg1 l (actualize error pos) pos (join_sign acc [sg1,Error])
      | _,_,_ -> failwith "sign_op error" ) in
    sign_op_rec 

let sign_comp (comp: comp) (int1: int) (int2: int): sign list * sign list SymbolTable.t * int * bool =
  match comp with
  | Eq -> int1 = int2
  | Ne -> int1 <> int2
  | Lt -> int1 < int2
  | Le -> int1 <= int2
  | Gt -> int1 > int2
  | Ge -> int1 >= int2

let sign_of_expr expr env error pos: sign list * int =
  match expr with
    | Num integer -> sign_of_int integer
    | Var name -> SymbolTable.find name env
    | Op (op, expr1, expr2) ->
      let (lst1,error1) = sign_of_expr expr1 in
      let (lst2,error2) = sign_of_expr expr2 in
      let firstError = getFirstError error1 error2 in
      sign_op op lst1 list2 firstError pos



let sign_of_cond cond env error pos: sign list * sign list SymbolTable.t * int * bool = failwith "TODO"
(* sign_of_expr des 2 expr (G et D) et on garde errorG si != 0 sinon errorD (getFirstError errorG errorD)
 puis verif si impossible alors return direct (pas de propagation)
 sinon ( result de la comparaison , result de propagation , errorD , false ) *)

let inv_of_cond cond: cond = failwith "TODO"



let sign_block (block: block): (sign list) SymbolTable.t * int =

  let rec sign_block_rec (block: block) (env: (sign list) SymbolTable.t) (error: int): (sign list) SymbolTable.t * int =
    match block with
    | [] -> (env,error)
    | (pos, instr)::block' ->
      let (env',error') =
        match instr with
        | Set (name, expr) ->
          let (sign2,error2) = sign_of_expr expr env error pos in
          let signRes  = join_sign sign2 (SymbolTable.find name env) in
          ((SymbolTable.add name signRes env), error2)
        | Print (expr) -> (env, error)
        | Read (name) -> ((SymbolTable.add name [Neg;Zero;Pos] env), error)
        | If (cond, if_block, else_block) ->
          let (signIF,envIF,errorIF,impossibleIF) = sign_of_cond cond env error pos in
          let (envIF2,errorIF2) =
          if (not impossibleIF) then
           sign_block_rec if_block envIF errorIF
          else (envIF,errorIF) in
          let (signELSE,envELSE,errorELSE,impossibleELSE) = sign_of_cond (inv_of_cond cond) env error pos in
          let (envELSE2,errorELSE2) =
          if (not impossibleELSE) then
            sign_block_rec else_block envELSE errorELSE
          else (envELSE,errorELSE) in
          if impossibleIF then
            (envELSE2,errorELSE2)
          else if impossibleELSE then
            (envIF2,errorIF2)
          else
            ((join_env envIF2 envELSE2),getFirstError errorIF2 errorELSE2)
        | While (cond, block) ->
          let rec parcour_while block cond env error =
            let (sign1,env1,error1,impossible1) = sign_of_cond cond env error pos in
            let (env2,error2) =
              if (not impossible1) then
                sign_block_rec block env1 error1
              else (env1,error1) in
            let envRes = join_env env env2 in
            let stop = equal_env env envRes in
            if stop then
              let (sign3,env3,error3,impossible3) = sign_of_cond (inv_of_cond cond) envRes error2 pos in
              (env3,error3)
            else parcour_while block cond envRes error2 in
          parcour_while block cond env error in
      sign_block_rec block' env' error' in

  sign_block_rec block SymbolTable.empty 0






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
  failwith "TODO"

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