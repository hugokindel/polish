open Ptypes
open Printf

module Names = Set.Make(String)

let rec search_expr (expr: expr) (acc1: Names.t) (acc2: Names.t): Names.t * Names.t =
  match expr with
  | Num integer -> (acc1,acc2)
  | Var name ->
  	let isEtu = Names.mem name acc1 in
      if not isEtu then
        let is_Non_Init = Names.mem name acc2 in
        if not is_Non_Init then
        	let acc2Res = Names.add name acc2 in
        	(acc1,acc2Res)
        else (acc1,acc2)
      else (acc1,acc2)
  | Op (op, expr1, expr2) ->
    let (acc1Res,acc2Res) = search_expr expr1 acc1 acc2 in
    search_expr expr2 acc1Res acc2Res

let search_cond (cond: cond) (acc1: Names.t) (acc2: Names.t): Names.t * Names.t =
  match cond with
  | (left_expr, comp, right_expr) ->
  	 let (acc1Res,acc2Res) = search_expr left_expr acc1 acc2 in
     search_expr right_expr acc1Res acc2Res

let search_block (block: block): Names.t * Names.t =

  let rec search_block_rec (block: block) (acc1: Names.t) (acc2: Names.t): Names.t * Names.t =
    match block with
    | [] -> (acc1,acc2)
    | (_, instr)::block' ->
        let (acc1End,acc2End) =
	      (match instr with
	      | Set (name, expr) ->
	      	let isEtu = Names.mem name acc1 in
	      	if not isEtu then
	      	  let acc1Res = Names.add name acc1 in
	      	  search_expr expr acc1Res acc2
	      	else search_expr expr acc1 acc2
	      | Print (expr) -> search_expr expr acc1 acc2
	      | Read (name) ->
	      	let isEtu = Names.mem name acc1 in
	      	if not isEtu then
	      	  let acc1Res = Names.add name acc1 in
	      	  (acc1Res,acc2)
	      	else (acc1,acc2)
	      | If (cond, if_block, else_block) ->
	        let (acc1Res,acc2Res) = search_cond cond acc1 acc2 in
	        let (acc1Res_2,acc2Res_2) = search_block_rec if_block acc1Res acc2Res in
	        let (acc1Res_3,acc2Res_3) = search_block_rec else_block acc1Res acc2Res_2 in
	        let acc1Final = Names.filter (fun elt -> Names.mem elt acc1Res_2) acc1Res_3 in
	        (acc1Final,acc2Res_3)
	      | While (cond, block) ->
	        let (acc1Res,acc2Res) = search_cond cond acc1 acc2 in
	        let (acc1Res_2,acc2Res_2) = search_block_rec block acc1Res acc2Res in
	        (acc1,acc2Res_2)    ) in
    	search_block_rec block' acc1End acc2End in

  search_block_rec block Names.empty Names.empty

  let print_vars acc1 acc2: unit =
  	Names.iter (fun elt -> printf "%s " elt) (Names.union acc1 acc2);
  	printf "\n";
  	Names.iter (fun elt -> printf "%s " elt) acc2;
  	printf "\n"