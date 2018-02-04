
(*IF-OCAML*)
type 'a start =
  | Module : (Script.var option * Script.definition) start
  | Script : Script.script start
  | Script1 : Script.script start

exception Syntax of Source.region * string

val parse : string -> Lexing.lexbuf -> 'a start -> 'a (* raises Syntax *)

val string_to_script : string -> Script.script (* raises Syntax *)
val string_to_module : string -> Script.definition (* raises Syntax *)

(*ENDIF-OCAML*)
(*F#
exception Syntax = Script.Syntax


[<AbstractClass>]
type start<'a> =
    class
    abstract member parse: string -> Lexer.Lexing.lexbuf -> 'a
    end

val Module : (Script.var option * Script.definition) start
val Script : Script.script start
val Script1 : Script.script start


val parse : string -> Lexer.Lexing.lexbuf -> 'a start -> 'a (* raises Syntax *)

val string_to_script : string -> Script.script (* raises Syntax *)
val string_to_module : string -> Script.definition (* raises Syntax *)

F#*)

