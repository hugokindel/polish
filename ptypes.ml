(** Line number in front of a line (starting at 1). *)
type position = int

(** Variable name. *)
type name = string

(** Arithmetic operators. *)
type op =
| Add (* + *)
| Sub (* - *)
| Mul (* * *)
| Div (* / *)
| Mod (* % *)

(** Arithmetic expressions. *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Comparison operators. *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** A condition is a comparison between two expressions. *)
type cond = expr * comp * expr

(** Instructions. *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
(** A block of instructions with line position. *)
and block = (position * instr) list

(** Polish program (a block of instruction). *)
type program = block

(** Number of spaces in front of a line. *)
type indentation = int

(** Line of file with useful informations (position and indentation). *)
type line = position * indentation * string list

(** Signs of a variable. *)
type sign =
| Neg
| Zero
| Pos
| Error