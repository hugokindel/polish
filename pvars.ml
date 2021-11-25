open Ptypes
open Printf

module SymbolSet = Set.Make(String)

let rec vars_expr (expr: expr) (init: SymbolSet.t) (uninit: SymbolSet.t): SymbolSet.t * SymbolSet.t =
  match expr with
  | Num integer -> (init, uninit)
  | Var name ->
    if SymbolSet.mem name init then (init, uninit)
    else
      if SymbolSet.mem name uninit then (init, uninit)
      else (init, (SymbolSet.add name uninit))
  | Op (op, expr1, expr2) ->
    let (init, uninit) = vars_expr expr1 init uninit in
    vars_expr expr2 init uninit

let vars_cond (cond: cond) (init: SymbolSet.t) (uninit: SymbolSet.t): SymbolSet.t * SymbolSet.t =
  match cond with
  | (left_expr, comp, right_expr) ->
  	let (init, uninit) = vars_expr left_expr init uninit in
    vars_expr right_expr init uninit

let vars_block (block: block): SymbolSet.t * SymbolSet.t =

  let rec vars_block_rec (block: block) (init: SymbolSet.t) (uninit: SymbolSet.t): SymbolSet.t * SymbolSet.t =
    match block with
    | [] -> (init, uninit)
    | (_, instr)::block' ->
      let (init, uninit) =
	      match instr with
	      | Set (name, expr) ->
	      	if SymbolSet.mem name init then vars_expr expr init uninit
	      	else vars_expr expr (SymbolSet.add name init) uninit
	      | Print (expr) -> vars_expr expr init uninit
	      | Read (name) ->
	      	if SymbolSet.mem name init then (init, uninit)
	      	else (SymbolSet.add name init, uninit)
	      | If (cond, if_block, else_block) ->
	        let (init, uninit) = vars_cond cond init uninit in
	        let (if_init, uninit) = vars_block_rec if_block init uninit in
	        let (else_init, uninit) = vars_block_rec else_block init uninit in
	        let init = SymbolSet.filter (fun elt -> SymbolSet.mem elt if_init) else_init in
	        (init, uninit)
	      | While (cond, block) ->
	        let (_, uninit) = vars_cond cond init uninit in
	        let (_, uninit) = vars_block_rec block init uninit in
	        (init, uninit) in
    	vars_block_rec block' init uninit in

  vars_block_rec block SymbolSet.empty SymbolSet.empty

let print_vars (init: SymbolSet.t) (uninit: SymbolSet.t): unit =
  SymbolSet.iter (fun elt -> printf "%s " elt) (SymbolSet.union init uninit);
  printf "\n";
  SymbolSet.iter (fun elt -> printf "%s " elt) uninit;
  printf "\n"