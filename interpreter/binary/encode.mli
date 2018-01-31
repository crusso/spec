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

val version : int32
val encode : Ast.module_ -> string

