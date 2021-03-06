open Types
open Values

(*IF-OCAML*)

type global
type t = global

exception Type
exception NotMutable

val alloc : global_type -> value -> global  (* raises Type *)
val type_of : global -> global_type

val load : global -> value
val store : global -> value -> unit  (* raises Type, NotMutable *)
(*ENDIF-OCAML*)
(*F#
type ``global``
type t = ``global``


exception Type
exception NotMutable

val alloc : global_type -> value -> ``global``  (* raises Type *)
val type_of : ``global`` -> global_type

val load : ``global`` -> value
val store : ``global``-> value -> unit  (* raises Type, NotMutable *)
F#*)


