(*F#
open FSharp.Compatibility.OCaml
F#*)
val instr : out_channel -> int -> Ast.instr -> unit
val func : out_channel -> int -> Ast.func -> unit
val module_ : out_channel -> int -> Ast.module_ -> unit
(*IF-OCAML*)
val script : out_channel -> int -> [`Textual | `Binary] -> Script.script -> unit
(*ENDIF-OCAML*)
(*F#
val script : out_channel -> int -> Arrange.kind -> Script.script -> unit
F#*)