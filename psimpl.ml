open Ptypes

(** TODO : Module pr des calculs precis ? *)
let result_of_op (op: op) (int1: int) (int2: int): int =
  match op with
  | Add -> int1 + int2
  | Sub -> int1 - int2
  | Mul -> int1 * int2
  | Div -> int1 / int2
  | Mod -> int1 mod int2


let rec simplification_expr expr : expr =
  match expr with
  | Num integer -> Num integer
  | Var name -> Var name
  | Op (op, expr1, expr2) ->
  		(match op, expr1, expr2 with
  			| _, Num int1, Num int2 ->
  				let result = result_of_op op int1 int2 in Num result
  			| Div, Num 0, _
  			| Mod, Num 0, _
  			| Mod, _, Num 1
  			| Mul, Num 0, _
  			| Mul, _, Num 0 -> Num 0
  			| Add, Num 0, _
  			| Mul, Num 1, _ -> simplification_expr expr2
  			| Div, _, Num 1
  			| Add, _, Num 0
  			| Mul, _, Num 1
  			| Sub, _, Num 0 -> simplification_expr expr1
  			| Mod, _, Num 0
  			| Div, _, Num 0 ->
  				let simplExpr1 = simplification_expr expr1 in
  				Op (op,simplExpr1,expr2)
  			| _,_,_ ->
  				let simplExpr1 = simplification_expr expr1 in
  				let simplExpr2 = simplification_expr expr2 in
  				if simplExpr1 = expr1 && simplExpr2 = expr2
  				then Op ( op , simplExpr1 , simplExpr2 )
  				else simplification_expr (Op ( op , simplExpr1 , simplExpr2 ) )

        )



let simplification_cond (cond: cond): cond =
  match cond with
  | (left_expr, comp, right_expr) ->
    ( (simplification_expr left_expr) , comp , (simplification_expr right_expr) )


let propa_block (block: block): block =

  let rec propa_block_rec block acc: block =
    match block with
    | [] -> acc
    | (position, instr)::block' ->
      let instr2 =
      (match instr with
      | Set (name, expr) -> Set (name,(simplification_expr expr))
      | Print (expr) -> Print (simplification_expr expr)
      | Read (name) -> Read (name)
      | If (cond, if_block, else_block) ->
        let cond2 = simplification_cond cond in
        let if_block2 = propa_block_rec if_block [] in
        let else_block2 = propa_block_rec else_block [] in
        If (cond2,if_block2,else_block2)
      | While (cond, block) ->
        let cond2 = simplification_cond cond in
        let block2 = propa_block_rec block [] in
        While (cond2,block2)  ) in
      propa_block_rec block' (acc@[(position,instr2)]) in

  propa_block_rec block []