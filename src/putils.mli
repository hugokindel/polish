open Ptypes

(* Evaluates a given `op` (arithmetic operator) of two integers and returns the result. *)
val eval_op : op -> int -> int -> int

(* Evaluates a given `comp` (comparison operator) of two integers and returns true if the condition is met. *)
val eval_comp : comp -> int -> int -> bool