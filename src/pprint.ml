open Printf
open Ptypes

(** Returns the string of an arithmetic operator. *)
let string_of_op (op: op): string =
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"

(** Returns the string of an expression. *)
let rec string_of_expr expr =
  match expr with
  | Num integer -> string_of_int integer
  | Var name -> name
  | Op (op, expr1, expr2) ->
    (string_of_op op) ^ " " ^ (string_of_expr expr1) ^ " " ^ (string_of_expr expr2)

(** Returns the string of a comparison operator. *)
let string_of_comp (comp: comp): string =
  match comp with
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

(** Returns the string of a condition. *)
let string_of_cond (cond: cond): string =
  match cond with
  | (left_expr, comp, right_expr) ->
    (string_of_expr left_expr) ^ " " ^ (string_of_comp comp) ^ " " ^ (string_of_expr right_expr)

(** Prints a line indentation to stdout. *)
let print_indent (indent: indentation): unit =

  let rec print_indent_rec (indent: indentation) (acc: string): unit =
    if indent > 0 then
      print_indent_rec (indent - 1) (acc ^ "  ")
    else
      print_string acc in

  print_indent_rec indent ""

(** Prints a Polish block to stdout. *)
let print_block (block: block): unit =

  let rec print_block_rec block indent: unit =
    match block with
    | [] -> ()
    | (_, instr)::block' ->
      print_indent indent;
      (match instr with
      | Set (name, expr) -> printf "%s := %s\n" name (string_of_expr expr);
      | Print (expr) -> printf "PRINT %s\n" (string_of_expr expr);
      | Read (name) -> printf "READ %s\n" name;
      | If (cond, if_block, else_block) ->
        printf "IF %s\n" (string_of_cond cond);
        print_block_rec if_block (indent + 1);
        (match else_block with
        | [] -> ()
        | _ ->
          (* If there is an `ELSE` block, print it too. *)
          print_indent indent;
          printf "ELSE\n";
          print_block_rec else_block (indent + 1));
      | While (cond, block) ->
        printf "WHILE %s\n" (string_of_cond cond);
        print_block_rec block (indent + 1));
      print_block_rec block' indent in

  print_block_rec block 0