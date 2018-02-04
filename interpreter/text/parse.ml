

(*IF-OCAML*)
type 'a start =
  | Module : (Script.var option * Script.definition) start
  | Script : Script.script start
  | Script1 : Script.script start

exception Syntax = Script.Syntax

let parse' name lexbuf start =
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = name};
  try start Lexer.token lexbuf
  with Syntax (region, s) ->
    let region' = if region <> Source.no_region then region else
      {Source.left = Lexer.convert_pos lexbuf.Lexing.lex_start_p;
       Source.right = Lexer.convert_pos lexbuf.Lexing.lex_curr_p} in
    raise (Syntax (region', s))

let parse (type a) name lexbuf : a start -> a = function
  | Module -> parse' name lexbuf Parser.module1
  | Script -> parse' name lexbuf Parser.script
  | Script1 -> parse' name lexbuf Parser.script1

let string_to start s =
  let lexbuf = Lexing.from_string s in
  parse "string" lexbuf start

let string_to_script s = string_to Script s
let string_to_module s = snd (string_to Module s)
(*ENDIF-OCAML*)
(*F#

type lexbuf = Lexer.Lexing.lexbuf

[<AbstractClass>]
type start<'a>() =
    class
    abstract member parse: string -> lexbuf -> 'a
    end

exception Syntax = Script.Syntax


let parse' name (lexbuf:lexbuf) start =
  lexbuf.EndPos <-
    {lexbuf.EndPos with pos_fname = name};
  try start Lexer.token lexbuf
  with Syntax (region, s) ->
    let region' = if region <> Source.no_region then region else
      {Source.left = Lexer.convert_pos lexbuf.StartPos;
       Source.right = Lexer.convert_pos lexbuf.EndPos} in
    raise (Syntax (region', s))


type Module() =
    class
    inherit start<Script.var option * Script.definition>()
    override this.parse name lexbuf = parse' name lexbuf Parser.module1
    end
type Script() =
    class
    inherit start<Script.script>()
    override this.parse name lexbuf = parse' name lexbuf Parser.script
    end
type Script1() =
    class
    inherit start<Script.script>()
    override this.parse name lexbuf = parse' name lexbuf Parser.script1
    end

let Module = Module() :> start<Script.var option * Script.definition>
let Script = Script() :> start<Script.script>
let Script1 = Script1() :> start<Script.script>

let parse name lexbuf : 'a start -> 'a = function
    start -> start.parse name lexbuf

let string_to start s =
  //let lexbuf = Lexing.from_string s in
  let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<char>.FromString(s) in
  parse "string" lexbuf start

let string_to_script s = string_to Script s
let string_to_module s = snd (string_to Module s)
F#*)

