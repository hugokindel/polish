
(** Projet Polish -- Analyse statique d'un mini-langage impératif *)

(** Note : cet embryon de projet est pour l'instant en un seul fichier
    polish.ml. Il est recommandé d'architecturer ultérieurement votre
    projet en plusieurs fichiers source de tailles raisonnables *)

(*****************************************************************************)
(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(***********************************************************************)


(** TODO : Fct pour read_polish [FAIT] *)

(** TODO : PRETRAITEMENT   [FAIT] *)


(** prend en arguments une liste de string et une liste vide puis renvoie la liste sans les mots vide *)
let rec menage l acc =
    match l with
    | [] -> acc
    | x::t -> ( match x with
                | "" -> menage t acc
                | _ -> menage t (acc@[x]) )

(** prend en arguments une liste de string et l'entier 0 puis renvoie l'indentation de la ligne *)
let rec indentation l acc =
    match l with
    | [] -> acc/2
    | x::t -> ( match x with
                | "" -> indentation t (acc+1)
                | _ -> acc/2 )

(** ligne d'un fichier : (numero de ligne, indentation, list des mots)  *)
type ligne = int * int * string list

(** prend en arguments un fichier ouvert et une liste vide et l'entier 1 puis renvoie la liste des lignes du fichier *)
let rec read_file ci (acc:ligne list) lg =
  try
    let x = String.split_on_char ' ' (input_line ci) in
    let ind = indentation x 0 in
    let s = menage x [] in
    let ligne = ((lg,ind,s):ligne) in
    read_file ci (acc@[ligne]) (lg+1)
  with End_of_file -> acc








(**  TODO : Fct pour Expressions arithmétiques ET les Conditions   [FAIT] *)

(** passe un string en list de char *)
let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

(** TODO : utilisez une autre méthode venant d'un module si ca existe ?
prend en argument une liste de char est dis si c'est un entier *)
let rec isInt s = match s with
    | [] -> true
    | x::t -> (match x with
                | '0'
                | '1'
                | '2'
                | '3'
                | '4'
                | '5'
                | '6'
                | '7'
                | '8'
                | '9' -> isInt t
                | _ -> false )

(** prend en argument une liste de string puis renvoie l'expression (et les lignes restantes) *)
let rec getExpr l = match l with
| [] -> failwith "Erreur getExpr"
| x::t -> (match x with
    | "+" -> (match t with
               | [] -> failwith "Erreur getExpr"
               | y::t2 ->let (exp,liRestant) = getExpr t in let (exp2,liRestant2) = getExpr liRestant in ((Op (Add,exp,exp2)),liRestant2)  )
    | "-" -> (match t with
               | [] -> failwith "Erreur getExpr"
               | y::t2 ->let (exp,liRestant) = getExpr t in let (exp2,liRestant2) = getExpr liRestant in ((Op (Sub,exp,exp2)),liRestant2)  )
    | "*" -> (match t with
               | [] -> failwith "Erreur getExpr"
               | y::t2 ->let (exp,liRestant) = getExpr t in let (exp2,liRestant2) = getExpr liRestant in ((Op (Mul,exp,exp2)),liRestant2)  )
    | "/" -> (match t with
               | [] -> failwith "Erreur getExpr"
               | y::t2 ->let (exp,liRestant) = getExpr t in let (exp2,liRestant2) = getExpr liRestant in ((Op (Div,exp,exp2)),liRestant2)  )
    | "%" -> (match t with
               | [] -> failwith "Erreur getExpr"
               | y::t2 ->let (exp,liRestant) = getExpr t in let (exp2,liRestant2) = getExpr liRestant in ((Op (Mod,exp,exp2)),liRestant2)  )
    | s -> if isInt (explode s) then ((Num (int_of_string s)),t) else ((Var (s:name)),t)
     )

(** prend en argument une liste de string puis renvoie la condition (et les lignes restantes) *)
let getCond l = match l with
| [] -> failwith "Erreur getCond"
| _ -> let (exp,liRestant) = getExpr l in
        (match liRestant with
          | [] -> failwith "Erreur getCond"
          | x::t -> (match x with
                      | "=" -> let (exp2,liRestant2) = getExpr t in (((exp,Eq,exp2):cond),liRestant2)
                      | "<>" -> let (exp2,liRestant2) = getExpr t in (((exp,Ne,exp2):cond),liRestant2)
                      | "<"-> let (exp2,liRestant2) = getExpr t in (((exp,Lt,exp2):cond),liRestant2)
                      | "<=" -> let (exp2,liRestant2) = getExpr t in (((exp,Le,exp2):cond),liRestant2)
                      | ">" -> let (exp2,liRestant2) = getExpr t in (((exp,Gt,exp2):cond),liRestant2)
                      | ">=" -> let (exp2,liRestant2) = getExpr t in (((exp,Ge,exp2):cond),liRestant2)
                      | _ -> failwith "Erreur getCond"   )   )






(** TODO : Fct pour obtenir une Instruction (READ,PRINT,SET) (WHILE) (IF) [FAIT] *)

(** prend en argument une liste de string puis renvoie l'instruction *)
let getInstruction l = match l with
    | [] -> failwith "Erreur getInstruction"
    | x::t -> (match x with
                | "READ" -> (match t with
                              | [] -> failwith "Erreur getInstruction READ"
                              | y::t2 -> Read (y:name) )
                | "PRINT" -> (match t with
                               | [] -> failwith "Erreur getInstruction PRINT"
                               | y::t2 -> let (exp,liRestant) = getExpr t in Print exp )
                | _ -> (match t with
                         | [] -> failwith "Erreur getInstruction"
                         | y::t2 -> (match y with
                                     | ":=" -> (match t2 with
                                                 | [] -> failwith "Erreur getInstruction :="
                                                 | y::t3 -> let (exp,liRestant) = getExpr t2 in Set ((x:name),exp)  )
                                     | _ -> failwith "Erreur getInstruction :=" )  )   )

(**  prend en argument une liste de string et 1 block puis renvoie l'instruction while *)
let getInstructionWhile sl block = match sl with
    | [] -> failwith "Erreur getInstrcutionWhile"
    | x::t -> (match x with
                | "WHILE" -> let (c,liRestant) = getCond t in While (c,block)
                | _ -> failwith "Erreur getInstrcutionWhile" )

(**  prend en argument une liste de string et 2 block puis renvoie l'instruction if*)
let getInstructionIf sl block block2 = match sl with
    | [] -> failwith "Erreur getInstrcutionIf"
    | x::t -> (match x with
                | "IF" -> let (c,liRestant) = getCond t in If (c,block,block2)
                | _ -> failwith "Erreur getInstrcutionIf" )










(**  TODO : PREMIERE VERSION, A SUPPRIMER

(** TODO return (block,lignes restantes) pour donner a getInstructionWhile sl block et avancer sur les lignes restantes *)
let rec getBlockWhile li indentation acc = match li with
    | [] -> (acc,li)
    | x::t -> (match x with
                | (lg,id,sl) ->
                    if id >= indentation then
                    (match List.hd sl with
                        | "WHILE" -> let (liBlock,liRestant) = getBlockWhile  t (indentation+1) [] in
                                     let inst = getInstructionWhile sl liBlock in
                                     getBlockWhile liRestant indentation (acc@[((lg:position),inst)])
                        | "IF" ->  let (liBlock,block2,liRestant) = getBlockIf  t (indentation+1) [] in
                                   let inst = getInstructionIf sl liBlock block2 in
                                   getBlockWhile liRestant indentation (acc@[((lg:position),inst)])
                        | "COMMENT" -> getBlockWhile t indentation acc
                        | _ ->  let inst = getInstruction sl in
                                getBlockWhile t indentation (acc@[((lg:position),inst)])  )
                    else (acc,li)     )


(** TODO   pourvoir utiliser getBlockIf ds getBlockWhile *)

(**  TODO function getBlockIf li indentation acc et return 2 block *)
let rec getBlockIf li indentation acc = match li with
    | [] -> (acc,li)
    | x::t -> (match x with
                | (lg,id,sl) ->
                    if id >= indentation then
                    (match List.hd sl with
                        | "WHILE" -> let (liBlock,liRestant) = getBlockWhile  t (indentation+1) [] in
                                     let inst = getInstructionWhile sl liBlock in
                                     getBlockIf liRestant indentation (acc@[((lg:position),inst)])
                        | "IF" ->  let (liBlock,block2,liRestant) = getBlockIf  t (indentation+1) [] in
                                   let inst = getInstructionIf sl liBlock block2 in
                                   getBlockIf liRestant indentation (acc@[((lg:position),inst)])
                        | "COMMENT" -> getBlockIf t indentation acc
                        | _ ->  let inst = getInstruction sl in
                                getBlockIf t indentation (acc@[((lg:position),inst)])  )
                    else (match List.hd sl with
                           | "ELSE" -> let (block2,liRestant) = getBlockIf t indentation [] in (acc,block2,liRestant)
                           | _ -> failwith "Erreur getBlockIf" ) )


*)







(** TODO : PREMIERE VERSION, A SUPPRIMER    (Tentative en 1 fct)

let rec getBlock li indentation acc isIf = match li with
    | [] -> (acc,li)
    | x::t -> (match x with
                | (lg,id,sl) ->
                    if id >= indentation then
                    (match List.hd sl with
                        | "WHILE" -> let (liBlock,liRestant) = getBlock  t (indentation+1) [] false in
                                     let inst = getInstructionWhile sl liBlock in
                                     getBlock liRestant indentation (acc@[((lg:position),inst)]) isIf
                        | "IF" ->  let (liBlock,block2,liRestant) = getBlock  t (indentation+1) [] true in
                                   let inst = getInstructionIf sl liBlock block2 in
                                   getBlock liRestant indentation (acc@[((lg:position),inst)]) isIf
                        | "COMMENT" -> getBlock t indentation acc isIf
                        | _ ->  let inst = getInstruction sl in
                                getBlock t indentation (acc@[((lg:position),inst)]) isIf )
                    else if isIf then
                    (match List.hd sl with
                           | "ELSE" -> let (block2,liRestant) = getBlock t indentation [] true in (acc,block2,liRestant)
                           | _ -> (acc,li) )
                    else (acc,li)   )

*)




(** TODO : Fct pour obtenir un Block  [FAIT] *)

(** prend en argument une liste de lignes, une indentation et un block vide
puis renvoie le block correspondant a l'indentation (contenant ceux d'indentation plus élevé) *)
let rec getBlock li indentation acc = match li with
    | [] -> (acc,li)
    | x::t -> (match x with
                | (lg,id,sl) ->
                    if id >= indentation then
                    (match List.hd sl with
                        | "WHILE" -> let (block,liRestant) = getBlock  t (indentation+1) [] in
                                     let inst = getInstructionWhile sl block in
                                     getBlock liRestant indentation (acc@[((lg:position),inst)])
                        | "IF" ->  let (block,liRestant) = getBlock  t (indentation+1) [] in
                                   (match liRestant with
                                     | [] ->  let inst = getInstructionIf sl block [] in
                                              getBlock liRestant indentation (acc@[((lg:position),inst)])
                                     | y::t2 ->  (match y with
                                                   | (lg2,id2,sl2) -> (match List.hd sl2 with
                                                                     | "ELSE" -> let (block2,liRestant2) = getBlock t2 (indentation+1) [] in
                                                                                 let inst = getInstructionIf sl block block2 in
                                                                                 getBlock liRestant2 indentation (acc@[((lg:position),inst)])
                                                                     | _ -> let inst = getInstructionIf sl block [] in
                                                                            getBlock liRestant indentation (acc@[((lg:position),inst)]) )  ) )
                        | "COMMENT" -> getBlock t indentation acc
                        | _ ->  let inst = getInstruction sl in
                                getBlock t indentation (acc@[((lg:position),inst)])  )
                    else (acc,li)   )








(**  TODO : Fct pour obtenir un Program   [FAIT] *)

(** prend en arguments une liste de lignes et un block vide puis renvoie un program *)
let rec getProgram li acc : program = match li with
    | [] -> acc
    | x::t -> (match x with
                | (lg,id,sl) ->
                    (match List.hd sl with
                        | "IF" -> let (block,liRestant) = getBlock t 1 [] in
                                  (match liRestant with
                                    | [] -> failwith "Erreur getProgram"
                                    | y::t2 ->  (match y with
                                                  | (lg2,id2,sl2) -> (match List.hd sl2 with
                                                                       | "ELSE" -> let (block2,liRestant2) = getBlock t2 1 [] in
                                                                                   let inst = getInstructionIf sl block block2 in
                                                                                   getProgram liRestant2 (acc@[((lg:position),inst)])
                                                                       | _ -> failwith "Erreur getProgram"  )  ) )
                        | "WHILE" -> let (block,liRestant) = getBlock t 1 [] in
                                     let inst = getInstructionWhile sl block in
                                     getProgram liRestant (acc@[((lg:position),inst)])
                        | "COMMENT" -> getProgram t acc
                        | _ -> let inst = getInstruction sl in
                               getProgram t (acc@[((lg:position),inst)]) )  )







(** TODO : PREMIERE VERSION, A SUPPRIMER

(** prend en arguments une liste de lignes et liste de (pos * instr) vide puis renvoie un program *)
let rec getProgram li acc = match li with
    | [] -> acc
    | x::t -> (match x with
                | (lg,id,sl) ->
                    (match List.hd sl with
                        | "IF" -> let (liBlock,blockk2,liRestant) = getBlockIf  t 1 [] in
                                  let inst = getInstructionIf sl liBlock block2 in
                                  getProgram liRestant (acc@[((lg:position),inst)])
                        | "WHILE" -> let (liBlock,liRestant) = getBlockWhile  t 1 [] in
                                     let inst = getInstructionWhile sl liBlock in
                                     getProgram liRestant (acc@[((lg:position),inst)])
                        | "COMMENT" -> getProgram t acc
                        | _ -> let inst = getInstruction sl in
                               getProgram t (acc@[((lg:position),inst)]) )  )


*)



(**  TODO : PREMIERE VERSION, A SUPPRIMER
prend en argument un nom de fichier puis renvoie le program
let read infile =
    try
      let ci = open_in infile in
      let li = read_file ci [] 1 in
      close_in ci;
      getProgram li
    with Sys_error s -> failwith "Erreur read file"
*)


(** TODO : Fct pour print_polish [A FAIRE] *)

(** TODO : Fct pour eval_polish [A FAIRE] *)

let read_polish (filename:string) : program =
   try
      let ci = open_in filename in
      let li = read_file ci [] 1 in
      close_in ci;
      getProgram li []
    with Sys_error s -> failwith "Erreur read file"

let print_polish (p:program) : unit = failwith "TODO"

let eval_polish (p:program) : unit = failwith "TODO"

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
