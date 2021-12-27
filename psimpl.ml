open Ptypes
open Putils

(** Returns a pair of boolean to said if first it can be dead and second the result of the condition. *)
let is_code_dead (cond: cond): bool * bool =
  let (left_expr, comp, right_expr) = cond in
  match left_expr, right_expr with
  | Num int1, Num int2 -> (true, (eval_comp comp int1 int2))
  | _, _ -> (false, false)

(** Returns a simplified expression. *)
let rec simpl_expr (expr: expr): expr =
  match expr with
  | Num integer -> Num integer
  | Var name -> Var name
  | Op (op, expr1, expr2) ->
  	match op, expr1, expr2 with
  	| _, Num int1, Num int2 -> Num (eval_op op int1 int2)
  	| Div, Num 0, _
  	| Mod, Num 0, _
  	| Mod, _, Num 1
  	| Mul, Num 0, _
  	| Mul, _, Num 0 -> Num 0
  	| Add, Num 0, _
  	| Mul, Num 1, _ -> simpl_expr expr2
  	| Div, _, Num 1
  	| Add, _, Num 0
  	| Mul, _, Num 1
  	| Sub, _, Num 0 -> simpl_expr expr1
  	| Mod, _, Num 0
  	| Div, _, Num 0 -> Op (op, (simpl_expr expr1), expr2)
  	| _,_,_ ->
  		let simpl_expr1 = simpl_expr expr1 in
  		let simpl_expr2 = simpl_expr expr2 in
  		(** If there is no modification we can't simplifie more *)
  		if simpl_expr1 = expr1 && simpl_expr2 = expr2 then
        Op (op, simpl_expr1, simpl_expr2)
        (** Else we need to check if we can simplifie more *)
  		else
        simpl_expr (Op (op, simpl_expr1, simpl_expr2))

(** Returns a simplified condition. *)
let simpl_cond (cond: cond): cond =
  match cond with
  | (left_expr, comp, right_expr) ->
    ((simpl_expr left_expr), comp, (simpl_expr right_expr))

(* Simplifies a block. *)
let simpl_block (block: block): block =

  let rec simpl_block_rec block acc: block =
    match block with
    | [] -> acc
    | (position, instr)::block' ->
      let simpl_block =
        match instr with
        | Set (name, expr) -> [(position, Set (name,(simpl_expr expr)))]
        | Print (expr) -> [(position, Print (simpl_expr expr))]
        | Read (name) -> [(position, Read (name))]
        | If (cond, if_block, else_block) ->
          let cond = (simpl_cond cond) in
          let (one_is_dead, else_is_dead) = is_code_dead cond in
          (** If one of the block can be dead. *)
          if one_is_dead then
            (** The condition is true. *)
            if else_is_dead then
              simpl_block_rec if_block []
            (** Else it's false. *)
            else
              simpl_block_rec else_block []
          (** Else we keep both. *)
          else
            let if_block = simpl_block_rec if_block [] in
            let else_block = simpl_block_rec else_block [] in
            [(position, If (cond, if_block, else_block))]
        | While (cond, block) ->
          let cond = simpl_cond cond in
          let (one_is_dead, else_is_dead) = is_code_dead cond in
          (** If the While block can be dead and his condition is false. *)
          if one_is_dead && (not else_is_dead) then []
          (** Else we keep it. *)
          else [(position, While (cond, (simpl_block_rec block [])))] in
      simpl_block_rec block' (acc @ simpl_block) in

  simpl_block_rec block []