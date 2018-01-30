



(*IF-OCAML*)
exception Code of Source.region * string
(*ENDIF-OCAML*)
(*F#
module Errors = sig
module Code =
sig
  exception Error of Source.region * string
end
end
exception Code = Errors.Code.Error
F#*)

val decode : string -> string -> Ast.module_ (* raises Code *)
