open Ptypes

let eval_op (op: op) (int1: int) (int2: int): int =
  match op with
  | Add -> int1 + int2
  | Sub -> int1 - int2
  | Mul -> int1 * int2
  | Div -> if int2 <> 0 then int1 / int2 else failwith "failed to evaluate `/`: division by zero"
  | Mod -> if int2 <> 0 then int1 mod int2 else failwith "failed to evaluate `%`: division by zero"