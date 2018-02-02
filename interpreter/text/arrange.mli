open Sexpr

val instr : Ast.instr -> sexpr
val func : Ast.func -> sexpr
val module_ : Ast.module_ -> sexpr

(*IF-OCAML*)
val script : [`Textual | `Binary] -> Script.script -> sexpr list
(*ENDIF-OCAML*)
(*F#
type kind;
val Textual : kind
val Binary: kind
val script : kind -> Script.script -> sexpr list
F#*)
