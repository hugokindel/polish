open Ptypes

(* Evaluates an op and returns the result. *)
let eval_op (op: op) (int1: int) (int2: int): int =
  match op with
  | Add -> int1 + int2
  | Sub -> int1 - int2
  | Mul -> int1 * int2
  | Div -> if int2 <> 0 then int1 / int2 else failwith "failed to evaluate `/`: division by zero"
  | Mod -> if int2 <> 0 then int1 mod int2 else failwith "failed to evaluate `%`: division by zero"

(* Evaluates a comp and returns true if the condition is met. *)
let eval_comp (comp: comp) (int1: int) (int2: int): bool =
  match comp with
  | Eq -> int1 = int2
  | Ne -> int1 <> int2
  | Lt -> int1 < int2
  | Le -> int1 <= int2
  | Gt -> int1 > int2
  | Ge -> int1 >= int2