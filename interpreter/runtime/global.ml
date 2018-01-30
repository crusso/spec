open Types
open Values

(*IF-OCAML*)
type global = {mutable content : value; mut : mutability}
type t = global
(*ENDIF-OCAML*)
(*F#
type ``global`` = {mutable content : value; mut : mutability}
type t = ``global``
F#*)




exception Type
exception NotMutable

let alloc (GlobalType (t, mut)) v =
  if type_of v <> t then raise Type;
  {content = v; mut = mut}

let type_of glob =
  GlobalType (type_of glob.content, glob.mut)

let load glob = glob.content
let store glob v =
  if glob.mut <> Mutable then raise NotMutable;
  if Values.type_of v <> Values.type_of glob.content then raise Type;
  glob.content <- v
