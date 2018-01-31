(*IF-OCAML*)
exception Invalid of Source.region * string
(*ENDIF-OCAML*)
(*F#
module Errors = sig
module Invalid =
sig
  exception Error of Source.region * string
end
end
exception Invalid = Errors.Invalid.Error
F#*)

val check_module : Ast.module_ -> unit (* raises Invalid *)
