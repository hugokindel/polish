open Printf
open Ptypes

module SymbolTable = Map.Make(String)

let sign_of_int (number: int): sign list =
  if number = 0 then
    [Zero]
  else
    if number < 0 then
      [Neg]
    else
      [Pos]

let exclusion_sign (l1: sign list) (l2: sign list): sign list =
  let rec exclusion_sign_rec l1 l2 acc: sign list =
    match l1 with
    | [] -> acc
    | x::t ->
      if List.mem x l2 then exclusion_sign_rec t l2 acc
      else exclusion_sign_rec t l2 (x::acc) in
  exclusion_sign_rec l1 l2 []

let join_sign (l1: sign list) (l2: sign list): sign list =
  let rec join_sign_rec l2 acc: sign list =
    match l2 with
    | [] -> acc
    | x::t ->
      if List.mem x acc then join_sign_rec t acc
      else join_sign_rec t (x::acc) in
  join_sign_rec l2 l1

let rec equal_sign (l1: sign list) (l2: sign list): bool =
    match l1 with
    | [] -> true
    | x::t -> if List.mem x l2 then equal_sign t l2 else false

let join_env (env1: (sign list) SymbolTable.t) (env2: (sign list) SymbolTable.t): (sign list) SymbolTable.t =
  SymbolTable.union (fun x y z -> Some (join_sign y z)) env1 env2

let actualize (error: int) (pos: position): int =
  if error != 0 then error else pos

let getFirstError (errorIF: int) (errorELSE: int): int =
  if errorIF != 0 then errorIF else errorELSE

let equal_env (env1: (sign list) SymbolTable.t) (env2: (sign list) SymbolTable.t): bool =
  SymbolTable.equal (fun x y -> equal_sign x y) env1 env2

let sign_op (op: op) (lst1: sign list) (lst2: sign list) (error: int) (pos: position): sign list * int =

  let rec verif_rec op sg1 lst2 error pos acc=
    match lst2 with
    | [] -> (acc,error)
    | y::l ->
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

  let rec sign_op_rec op lst1 lst2 error pos acc =
    match lst1 with
    | [] -> (acc,error)
    | x::t ->
      let (accSg1,newError) = verif_rec op x lst2 error pos [] in
      sign_op_rec op t lst2 newError pos (join_sign acc accSg1) in

  sign_op_rec op lst1 lst2 error pos []

let rec sign_of_expr (expr: expr) (env: (sign list) SymbolTable.t) (error: int) (pos: position): sign list * int =
  match expr with
  | Num integer -> (sign_of_int integer,error)
  | Var name -> (SymbolTable.find name env,error)
  | Op (op, expr1, expr2) ->
    let (lst1,error1) = sign_of_expr expr1 env error pos in
    let (lst2,error2) = sign_of_expr expr2 env error1 pos in
    sign_op op lst1 lst2 error2 pos

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
        | Pos -> if List.mem Pos lst2 then false else lt_rec t lst2
        | Zero -> if List.mem Pos lst2 then false else lt_rec t lst2
        | Neg -> if List.mem Pos lst2 || List.mem Zero lst2 then false else lt_rec t lst2
        | Error -> lt_rec t lst2 ) ) in

  let rec gt_rec lst1 lst2 =
    (match lst1 with
     | [] -> true
     | x::t ->
       (match x with
        | Pos -> if List.mem Neg lst2 || List.mem Zero lst2 then false else gt_rec t lst2
        | Zero -> if List.mem Neg lst2 then false else gt_rec t lst2
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

let propagation_env (cond: cond) (env: (sign list) SymbolTable.t) (error: int) (pos: position): (sign list) SymbolTable.t =

  let (left_expr, comp, right_expr) = cond in

  let propa name comp expr2 env error pos =
    let (lst2,error2) = sign_of_expr expr2 env error pos in
      match comp with
      | Eq ->
        let signRes = join_sign lst2 (SymbolTable.find name env) in
        SymbolTable.add name signRes env
      | Ne ->
        let signRes = exclusion_sign (SymbolTable.find name env) lst2 in
        SymbolTable.add name signRes env
      | Le
      | Lt ->
        if List.mem Pos lst2 then
          env
        else if List.mem Zero lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Pos] in
          SymbolTable.add name signRes env
        else
          let signRes = exclusion_sign (SymbolTable.find name env) [Pos;Zero] in
          SymbolTable.add name signRes env
      | Ge
      | Gt ->
        if List.mem Neg lst2 then
          env
        else if List.mem Zero lst2 then
          let signRes = exclusion_sign (SymbolTable.find name env) [Neg] in
          SymbolTable.add name signRes env
        else
          let signRes = exclusion_sign (SymbolTable.find name env) [Neg;Zero] in
          SymbolTable.add name signRes env  in

  match left_expr with
  | Var name -> propa name comp right_expr env error pos
  | _ ->
    match right_expr with
    | Var name ->
      let reverse_comp comp =
        (match comp with
         | Eq -> Eq
         | Ne -> Ne
         | Lt -> Gt
         | Le -> Ge
         | Gt -> Lt
         | Ge -> Le ) in
       propa name (reverse_comp comp) left_expr env error pos
    | _ -> env

let sign_of_cond (cond: cond) (env: (sign list) SymbolTable.t) (error: int) (pos: position): (sign list) SymbolTable.t * int * bool =
  let (isImpossible,newError) = plausibility_cond cond env error pos in
  if isImpossible then (env,newError,isImpossible)
  else let propaEnv = propagation_env cond env newError pos in (propaEnv,newError,isImpossible)

let sign_block (block: block): (sign list) SymbolTable.t * int =

  let rec sign_block_rec (block: block) (env: (sign list) SymbolTable.t) (error: int): (sign list) SymbolTable.t * int =
    match block with
    | [] -> (env,error)
    | (pos, instr)::block' ->
      let (env',error') =
        match instr with
        | Set (name, expr) ->
          let exist = SymbolTable.mem name env in
          let (sign2,error2) = sign_of_expr expr env error pos in
          let signRes  =
          if exist then
            join_sign (SymbolTable.find name env) sign2
          else
            sign2 in
          ((SymbolTable.add name signRes env), error2)
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

let rec print_sign = function
  | [] -> ()
  | x::t -> (match x with
             | Neg -> printf "-"
             | Zero -> printf "0"
             | Pos -> printf "+"
             | Error -> printf "!" ); print_sign t

let print_sign (env: (sign list) SymbolTable.t) (error: int): unit =
  SymbolTable.iter (fun key -> fun value -> printf "%s " key; print_sign value; printf "\n") env;
  if error != 0 then printf "divbyzero %d\n" error else printf "safe\n"
