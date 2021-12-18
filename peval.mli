open Ptypes

module SymbolTable : Map.S with type key = String.t

(* Evaluates a Polish `block` (block of Polish parsed code) and returns an environment. *)
val eval_block : block -> int SymbolTable.t