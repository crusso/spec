(*IF-OCAML*)
exception Unknown of Source.region * string

val link : Ast.module_ -> Instance.extern list (* raises Unknown *)

val register :
  Ast.name ->
  (Ast.name -> Types.extern_type -> Instance.extern (* raises Not_found *)) ->
  unit
(*ENDIF-OCAML*)
(*F#



module Errors = sig
module Unknown =
sig
  exception Error of Source.region * string
end
end
exception Unknown = Errors.Unknown.Error

val link : Ast.module_ -> Instance.``extern`` list (* raises Unknown *)

val register :
  Ast.name ->
  (Ast.name -> Types.extern_type -> Instance.``extern`` (* raises Not_found *)) ->
  unit
F#*)

