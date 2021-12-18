open Ptypes

module SymbolSet : Set.S with type elt = String.t

(* Analyze a `block` (block of Polish code)  *)
val vars_block : block -> SymbolSet.t * SymbolSet.t

(* Prints all variables of a set to know which ones were uninitialized after access and returns nothing (a unit). *)
val print_vars : SymbolSet.t -> SymbolSet.t -> unit