open Values
open Instance




(*IF-OCAML*)
exception Link of Source.region * string
exception Trap of Source.region * string
exception Crash of Source.region * string
exception Exhaustion of Source.region * string
(*ENDIF-OCAML*)
(*F#
module Errors = sig
module Link =
sig
  exception Error of Source.region * string
end
module Trap = 
sig
  exception Error of Source.region * string
end
module Crash =
sig
  exception Error of Source.region * string
end
module Exhaustion =
sig
  exception Error of Source.region * string
end
end
exception Link = Errors.Link.Error
exception Trap = Errors.Trap.Error
exception Crash = Errors.Crash.Error 
exception Exhaustion = Errors.Exhaustion.Error
F#*)


(*IF-OCAML*)
val init : Ast.module_ ->  extern list -> module_inst (* raises Link, Trap *)
(*ENDIF-OCAML*)
(*F#
val init : Ast.module_ ->  ``extern`` list -> module_inst (* raises Link, Trap *)
F#*)
val invoke : func_inst -> value list -> value list (* raises Trap *)
