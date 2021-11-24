open Ptypes

(** TODO : Module pr des calculs precis ? *)
let result_of_op (op: op) (int1: int) (int2: int): int =
  match op with
  | Add -> int1 + int2
  | Sub -> int1 - int2
  | Mul -> int1 * int2
  | Div -> int1 / int2
  | Mod -> int1 mod int2


let rec simplification_expr (expr: expr) : expr =
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


let result_of_cond (lexpr: expr) (comp: comp) (rexpr: expr): bool =
  match comp with
    | Eq -> lexpr = rexpr
    | Ne -> lexpr <> rexpr
    | Lt -> lexpr < rexpr
    | Le -> lexpr <= rexpr
    | Gt -> lexpr > rexpr
    | Ge -> lexpr >= rexpr


let dead_cond (cond: cond): bool * bool =
  match cond with
  | (left_expr, comp, right_expr) ->
      (match left_expr, right_expr with
        | Num int1, Num int2 -> 
            let result = result_of_cond left_expr comp right_expr in
            (true,result)
        | _, _ -> (false,false)  )


let propa_block (block: block): block =

  let rec propa_block_rec block acc: block =
    match block with
    | [] -> acc
    | (position, instr)::block' ->
      let blockResult =
      (match instr with
      | Set (name, expr) -> [(position , Set (name,(simplification_expr expr)) )]
      | Print (expr) -> [(position , Print (simplification_expr expr) )]
      | Read (name) -> [(position , Read (name) )]
      | If (cond, if_block, else_block) ->
        let cond2 = simplification_cond cond in
        let (b1,b2) = dead_cond cond2 in
        if b1 then
          if b2 then
            propa_block_rec if_block [] 
          else
            propa_block_rec else_block []
        else
          let if_block2 = propa_block_rec if_block [] in
          let else_block2 = propa_block_rec else_block [] in
          [(position , If (cond2,if_block2,else_block2) )]
      | While (cond, block) ->
        let cond2 = simplification_cond cond in
        let (b1,b2) = dead_cond cond2 in
        if b1 && (not b2) then
          []
        else
          let block2 = propa_block_rec block [] in
          [(position , While (cond2,block2) )]            ) in
      propa_block_rec block' (acc@blockResult) in

  propa_block_rec block []