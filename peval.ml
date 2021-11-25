open Ptypes
open Printf

(** TODO : use zarith module for expr and cond *)

module NameTable = Map.Make(String)


let res_of_op (op: op) (int1: int) (int2: int): int =
  match op with
  | Add -> int1 + int2
  | Sub -> int1 - int2
  | Mul -> int1 * int2
  | Div -> int1 / int2
  | Mod -> int1 mod int2


let checkExpr (expr: expr): unit =
	match expr with
	  | Op (op, expr1, expr2) ->
	  		(match op, expr1, expr2 with
				| Mod, _, Num 0
	  			| Div, _, Num 0 ->
	  				printf "Erreur division/modulo par 0 !\n";
	  				exit 2
	  			| _,_,_ -> () )
	  | _ -> ()


let rec res_of_expr (expr: expr) (env: int NameTable.t): expr =
  match expr with
	  | Num integer -> Num integer
	  | Var name -> Num (NameTable.find name env)
	  | Op (op, expr1, expr2) ->
	  		(match op, expr1, expr2 with
	  			| _, Num int1, Num int2 ->
	  				let result = res_of_op op int1 int2 in Num result
	  			| Div, Num 0, _
	  			| Mod, Num 0, _
	  			| Mod, _, Num 1
	  			| Mul, Num 0, _
	  			| Mul, _, Num 0 -> Num 0
	  			| Add, Num 0, _
	  			| Mul, Num 1, _ -> res_of_expr expr2 env
	  			| Div, _, Num 1
	  			| Add, _, Num 0
	  			| Mul, _, Num 1
	  			| Sub, _, Num 0 -> res_of_expr expr1 env
	  			| Mod, _, Num 0
	  			| Div, _, Num 0 ->
	  				let simplExpr1 = res_of_expr expr1 env in
	  				Op (op,simplExpr1,expr2)
	  			| _,_,_ ->
	  				let simplExpr1 = res_of_expr expr1 env in
	  				let simplExpr2 = res_of_expr expr2 env in
	  				if simplExpr1 = expr1 && simplExpr2 = expr2
	  				then Op ( op , simplExpr1 , simplExpr2 )
	  				else res_of_expr (Op ( op , simplExpr1 , simplExpr2 ) ) env )


let rec val_of_expr (expr: expr) (env: int NameTable.t): int =
  match expr with
	  | Num integer -> integer
	  | Var name -> NameTable.find name env
	  | Op (op, expr1, expr2) ->
	  		(match op, expr1, expr2 with
	  			| Div, Num 0, _
	  			| Mod, Num 0, _
	  			| Mod, _, Num 1
	  			| Mul, Num 0, _
	  			| Mul, _, Num 0 -> 0
	  			| Add, Num 0, _
	  			| Mul, Num 1, _ -> val_of_expr expr2 env
	  			| Div, _, Num 1
	  			| Add, _, Num 0
	  			| Mul, _, Num 1
	  			| Sub, _, Num 0 -> val_of_expr expr1 env
	  			| _, Num int1, Num int2 ->
	  				res_of_op op int1 int2
	  			| Mod, _, Num 0
	  			| Div, _, Num 0
	  			| _,_,_ -> failwith "Erreur val_of_expr"   )


let checkCond (cond: cond) (env: int NameTable.t): unit =
  match cond with
	| (left_expr, comp, right_expr) ->
		let simplExprLeft = res_of_expr left_expr env in
		let simplExprRight = res_of_expr left_expr env in
		checkExpr simplExprLeft;
		checkExpr simplExprRight


let res_of_cond (cond: cond) (env: int NameTable.t): bool =
  match cond with
	| (left_expr, comp, right_expr) ->
		let lexprT = res_of_expr left_expr env in
		let rexprT = res_of_expr right_expr env in
		let lexpr = val_of_expr lexprT env in
		let rexpr = val_of_expr rexprT env in
		(match comp with
		    | Eq -> lexpr = rexpr
		    | Ne -> lexpr <> rexpr
		    | Lt -> lexpr < rexpr
		    | Le -> lexpr <= rexpr
		    | Gt -> lexpr > rexpr
		    | Ge -> lexpr >= rexpr  )


let eval_block (block: block): int NameTable.t =

  let rec eval_block_rec (block: block) (env: int NameTable.t): int NameTable.t =
    match block with
    | [] -> env
    | (_, instr)::block' ->
      let newEnv =
	      (match instr with
	      | Set (name, expr) ->
	      	let res = res_of_expr expr env in
	      	checkExpr res;
	      	let res2 = val_of_expr res env in
	      	NameTable.add name res2 env
	      | Print (expr) ->
	      	let res = res_of_expr expr env in
	      	checkExpr res;
	      	let res2 = val_of_expr res env in
	      	printf "%i \n" res2;
	      	env
	      | Read (name) ->
	      	printf "%s ? " name;
	      	let res = read_int () in
	      	NameTable.add name res env
	      | If (cond, if_block, else_block) ->
	        checkCond cond env;
	        let resCond = res_of_cond cond env in
	        if resCond then
	        	eval_block_rec if_block env
	    	else
	    		eval_block_rec else_block env
	      | While (cond, block) ->
		    let rec parcour_while block cond env =
		      checkCond cond env;
			  let resCond = res_of_cond cond env in
			  if resCond then
		 	    let newEnv = eval_block_rec block env in
		 		parcour_while block cond newEnv
			  else env  in
	      	parcour_while block cond env          ) in
      eval_block_rec block' newEnv in

  eval_block_rec block NameTable.empty
