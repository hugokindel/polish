open Ptypes

(* Reads a `line list` (list of lines) from a `string` (filepath). *)
val read_file : string -> line list

(* Parses a Polish `block` (block of Polish parsed code) from a `line list` (list of line). *)
val parse_block : line list -> block