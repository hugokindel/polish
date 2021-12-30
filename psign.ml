open Printf
open Ptypes

module SymbolTable = Map.Make(String)

(** Returns the sign of an integer. *)
let sign_of_int (number: int): sign list =
  if number = 0 then
    [Zero]
  else
    if number < 0 then
      [Neg]
    else
      [Pos]

(** Returns the sign list of 'l1' without signs of 'l2'. *)
let exclusion_sign (l1: sign list) (l2: sign list): sign list =
  let rec exclusion_sign_rec l1 l2 acc: sign list =
    match l1 with
    | [] -> acc
    | x::t ->
      if List.mem x l2 then exclusion_sign_rec t l2 acc
      else exclusion_sign_rec t l2 (x::acc) in
  exclusion_sign_rec l1 l2 []

(** Returns the sign list of 'l1' with signs of 'l2'. *)
let join_sign (l1: sign list) (l2: sign list): sign list =
  let rec join_sign_rec l2 acc: sign list =
    match l2 with
    | [] -> acc
    | x::t ->
      if List.mem x acc then join_sign_rec t acc
      else join_sign_rec t (x::acc) in
  join_sign_rec l2 l1

(** Returns true if two sign list are equals. *)
let rec equal_sign (l1: sign list) (l2: sign list): bool =
    match l1 with
    | [] -> true
    | x::t -> if List.mem x l2 then equal_sign t l2 else false

(** Returns the union of two environment. *)
let join_env (env1: (sign list) SymbolTable.t) (env2: (sign list) SymbolTable.t): (sign list) SymbolTable.t =
  SymbolTable.union (fun x y z -> Some (join_sign y z)) env1 env2

(** Returns the error position if it's the first we see. *)
let actualize (error: int) (pos: position): int =
  if error != 0 then error else pos

(** Returns the first error we see. *)
let getFirstError (errorIF: int) (errorELSE: int): int =
  if errorIF != 0 then errorIF else errorELSE

(** Returns true if two environment are equals. *)
let equal_env (env1: (sign list) SymbolTable.t) (env2: (sign list) SymbolTable.t): bool =
  SymbolTable.equal (fun x y -> equal_sign x y) env1 env2

(** Returns the sign list and error of an operation. *)
let sign_op (op: op) (lst1: sign list) (lst2: sign list) (error: int) (pos: position): sign list * int =

  (* Compare a sign with a sign list. *)
  let rec verif_rec op sg1 lst2 error pos acc=
    match lst2 with
    | [] -> (acc,error)
    | y::l ->
      (* Comparison made one at a time. *)
      match op, sg1, y with
      | Mod, Neg, Pos
      | Mod, Pos, Pos
      | Div, Neg, Neg
      | Div, Pos, Pos
      | Mul, Neg, Neg
      | Mul, Pos, Pos
      | Sub, Pos, Neg
      | Sub, Pos, Zero
      | Sub, Zero, Neg
      | Add, Pos, Pos
      | Add, Pos, Zero
      | Add, Zero, Pos ->
        verif_rec op sg1 l error pos (join_sign acc [Pos])
      | Mod, Zero, Neg
      | Mod, Zero, Pos
      | Div, Zero, Neg
      | Div, Zero, Pos
      | Mul, Neg, Zero
      | Mul, Pos, Zero
      | Mul, Zero, _
      | Sub, Zero, Zero
      | Add, Zero, Zero ->
        verif_rec op sg1 l error pos (join_sign acc [Zero])
      | Sub, Pos, Pos
      | Sub, Neg, Neg
      | Add, Pos, Neg
      | Add, Neg, Pos ->
        verif_rec op sg1 l error pos (join_sign acc [Pos;Zero;Neg])
      | Mod, Neg, Neg
      | Mod, Pos, Neg
      | Div, Neg, Pos
      | Div, Pos, Neg
      | Mul, Neg, Pos
      | Mul, Pos, Neg
      | Sub, Zero, Pos
      | Sub, Neg, _
      | Add, Zero, Neg
      | Add, Neg, _ ->
        verif_rec op sg1 l error pos (join_sign acc [Neg])
      | Mod, _, Zero
      | Div, _, Zero ->
        verif_rec op sg1 l (actualize error pos) pos (join_sign acc [sg1;Error])
      | _, Error, _
      | _, _, Error ->
        verif_rec op sg1 l error pos (join_sign acc [Error]) in

  (* Compare two sign list. *)
  let rec sign_op_rec op lst1 lst2 error pos acc =
    match lst1 with
    | [] -> (acc,error)
    | x::t ->
      (* Comparison made one at a time. *)
      let (accSg1,newError) = verif_rec op x lst2 error pos [] in
      sign_op_rec op t lst2 newError pos (join_sign acc accSg1) in

  sign_op_rec op lst1 lst2 error pos []

(** Returns the sign list and error of an expression. *)
let rec sign_of_expr (expr: expr) (env: (sign list) SymbolTable.t) (error: int) (pos: position): sign list * int =
  match expr with
  | Num integer -> (sign_of_int integer,error)
  | Var name -> (SymbolTable.find name env,error)
  | Op (op, expr1, expr2) ->
    let (lst1,error1) = sign_of_expr expr1 env error pos in
    let (lst2,error2) = sign_of_expr expr2 env error1 pos in
    sign_op op lst1 lst2 error2 pos

(** Returns the inverse of a condition. *)
let inv_of_cond (cond: cond): cond =
  let inv_of_comp (comp: comp): comp =
    match comp with
    | Eq -> Ne
    | Ne -> Eq
    | Lt -> Ge
    | Le -> Gt
    | Gt -> Le
    | Ge -> Lt in

  let (left_expr, comp, right_expr) = cond in
  (left_expr,(inv_of_comp comp),right_expr)

(** Returns if a condition is impossible and the new error. *)
let plausibility_cond (cond: cond) (env: (sign list) SymbolTable.t) (error: int) (pos: position): bool * int =

  let (left_expr, comp, right_expr) = cond in
  let (lst1,error1) = sign_of_expr left_expr env error pos in
  let (lst2,error2) = sign_of_expr right_expr env error1 pos in

  let rec eq_rec lst1 lst2 =
    (match lst1 with
     | [] -> true
     | x::t ->
       if List.mem x lst2 then false
       else eq_rec t lst2 ) in

  let rec ne_rec lst1 lst2 =
    (match lst1 with
     | [] -> true
     | x::t ->
       if List.mem x lst2 then ne_rec t lst2
       else false ) in

  let rec lt_rec lst1 lst2 =
    (match lst1 with
     | [] -> true
     | x::t ->
       (match x with
        | Pos
        | Zero -> if List.mem Pos lst2 then false else lt_rec t lst2
        | Neg -> false
        | Error -> lt_rec t lst2 ) ) in

  let rec gt_rec lst1 lst2 =
    (match lst1 with
     | [] -> true
     | x::t ->
       (match x with
        | Pos -> false
        | Zero
        | Neg -> if List.mem Neg lst2 then false else gt_rec t lst2
        | Error -> gt_rec t lst2 ) ) in

  let rec le_rec lst1 lst2 =
    (match lst1 with
     | [] -> true
     | x::t ->
       (match x with
        | Pos -> if List.mem Pos lst2 then false else le_rec t lst2
        | Zero -> if List.mem Pos lst2 || List.mem Zero lst2 then false else le_rec t lst2
        | Neg -> false
        | Error -> le_rec t lst2 ) ) in

  let rec ge_rec lst1 lst2 =
    (match lst1 with
     | [] -> true
     | x::t ->
       (match x with
        | Pos -> false
        | Zero -> if List.mem Neg lst2 || List.mem Zero lst2 then false else ge_rec t lst2
        | Neg -> if List.mem Neg lst2 then false else ge_rec t lst2
        | Error -> ge_rec t lst2 ) ) in

   match comp with
   | Eq -> (eq_rec lst1 lst2,error2)
   | Ne -> (ne_rec lst1 lst2,error2)
   | Lt -> (lt_rec lst1 lst2,error2)
   | Le -> (le_rec lst1 lst2,error2)
   | Gt -> (gt_rec lst1 lst2,error2)
   | Ge -> (ge_rec lst1 lst2,error2)

(** Returns an environment after propagation of a condition. *)
let propagation_env (cond: cond) (env: (sign list) SymbolTable.t) (error: int) (pos: position): (sign list) SymbolTable.t =

  let (left_expr, comp, right_expr) = cond in

  (* Returns the new environment of the variable 'name' when we match Var comp expr2. *)
  let propa name comp expr2 env error pos =
    let (lst2,error2) = sign_of_expr expr2 env error pos in
      match comp with
      | Eq ->
        let signRes = join_sign lst2 (SymbolTable.find name env) in
        SymbolTable.add name signRes env
      | Ne ->
        let signRes = exclusion_sign (SymbolTable.find name env) lst2 in
        SymbolTable.add name signRes env
      | Le ->
        if List.mem Pos lst2 then
          env
        else if List.mem Zero lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Pos] in
          SymbolTable.add name signRes env
        else
          let signRes = exclusion_sign (SymbolTable.find name env) [Pos;Zero] in
          SymbolTable.add name signRes env
      | Lt ->
        if List.mem Pos lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Pos] in
          SymbolTable.add name signRes env
        else if List.mem Zero lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Pos;Zero] in
          SymbolTable.add name signRes env
        else
          env
      | Ge ->
        if List.mem Neg lst2 then
          env
        else if List.mem Zero lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Neg] in
          SymbolTable.add name signRes env
        else
          let signRes = exclusion_sign (SymbolTable.find name env) [Neg;Zero] in
          SymbolTable.add name signRes env
      | Gt ->
        if List.mem Neg lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Neg] in
          SymbolTable.add name signRes env
        else if List.mem Zero lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Neg;Zero] in
          SymbolTable.add name signRes env
        else
          env in

  (* going from left_expr comp Var to Var (reverse_comp) left_expr. *)
  let reverse_comp comp =
    (match comp with
     | Eq -> Eq
     | Ne -> Ne
     | Lt -> Gt
     | Le -> Ge
     | Gt -> Lt
     | Ge -> Le ) in

  match left_expr, right_expr with
  | Var nameL, Var nameR ->
    let newEnv = propa nameL comp right_expr env error pos in
    propa nameR (reverse_comp comp) left_expr newEnv error pos
  | Var name, _ -> propa name comp right_expr env error pos
  | _, Var name -> propa name (reverse_comp comp) left_expr env error pos
  | _, _ -> env

(** Returns the new environment, error and if the condition is possible. *)
let sign_of_cond (cond: cond) (env: (sign list) SymbolTable.t) (error: int) (pos: position): (sign list) SymbolTable.t * int * bool =
  let (isImpossible,newError) = plausibility_cond cond env error pos in
  if isImpossible then (env,newError,isImpossible)
  else let propaEnv = propagation_env cond env newError pos in (propaEnv,newError,isImpossible)

(* Analyze a block.  *)
let sign_block (block: block): (sign list) SymbolTable.t * int =

  let rec sign_block_rec (block: block) (env: (sign list) SymbolTable.t) (error: int): (sign list) SymbolTable.t * int =
    match block with
    | [] -> (env,error)
    | (pos, instr)::block' ->
      let (env',error') =
        match instr with
        | Set (name, expr) ->
          let (sign2,error2) = sign_of_expr expr env error pos in
          ((SymbolTable.add name sign2 env), error2)
        | Print (expr) -> (env, error)
        | Read (name) -> ((SymbolTable.add name [Neg;Zero;Pos] env), error)
        | If (cond, if_block, else_block) ->

          let (envIF,errorIF,impossibleIF) = sign_of_cond cond env error pos in
          let (envIF2,errorIF2) =
          if (not impossibleIF) then
           sign_block_rec if_block envIF errorIF
          else (envIF,errorIF) in

          let (envELSE,errorELSE,impossibleELSE) = sign_of_cond (inv_of_cond cond) env error pos in
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

          let rec parcour_while block cond env error pos =
            let (env1,error1,impossible1) = sign_of_cond cond env error pos in
            let (env2,error2) =
              if (not impossible1) then
                sign_block_rec block env1 error1
              else (env1,error1) in
            let envRes = join_env env env2 in
            let stop = equal_env env envRes in
            if stop then
              let (env3,error3,impossible3) = sign_of_cond (inv_of_cond cond) envRes error2 pos in
              (env3,error3)
            else parcour_while block cond envRes error2 pos in

          parcour_while block cond env error pos in
      sign_block_rec block' env' error' in

  sign_block_rec block SymbolTable.empty 0

(* Prints all signs of a list. *)
let rec print_sign = function
  | [] -> ()
  | x::t -> (match x with
             | Neg -> printf "-"
             | Zero -> printf "0"
             | Pos -> printf "+"
             | Error -> printf "!" ); print_sign t

(* Prints all variables of a Map to know their signs
   and print in the last line if the program is safe or not. *)
let print_sign (env: (sign list) SymbolTable.t) (error: int): unit =
  SymbolTable.iter (fun key -> fun value -> printf "%s " key; print_sign value; printf "\n") env;
  if error != 0 then printf "divbyzero %d\n" error else printf "safe\n"
