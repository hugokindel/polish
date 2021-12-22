open Ptypes

module SymbolTable : Map.S with type key = String.t

(* Analyze a `block` (block of Polish code)  *)
val sign_block : block -> (sign list) SymbolTable.t * int

(* Prints all variables of a Map to know their signs
   and print in the last line if the program is safe or not.
   Returns nothing (a unit). *)
val print_sign : (sign list) SymbolTable.t -> int -> unit