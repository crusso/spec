(*F#
open FSharp.Compatibility.OCaml
F#*)

let name = "wasm"
let version = "1.0"

let configure () =
  Import.register (Utf8.decode "spectest") Spectest.lookup;
  Import.register (Utf8.decode "env") Env.lookup

let banner () =
  print_endline (name ^ " " ^ version ^ " reference interpreter")

let usage = "Usage: " ^ name ^ " [option] [file ...]"

let args = ref []
let add_arg source = args := !args @ [source]

let quote s = "\"" ^ String.escaped s ^ "\""

(*IF-OCAML*)
let argspec = Arg.align
(*ENDIF-OCAML*)
(*F#
// TBR
let argspec =
F#*)
[
  "-", Arg.Set Flags.interactive,
    " run interactively (default if no files given)";
  "-e", Arg.String add_arg, " evaluate string";
  "-i", Arg.String (fun file -> add_arg ("(input " ^ quote file ^ ")")),
    " read script from file";
  "-o", Arg.String (fun file -> add_arg ("(output " ^ quote file ^ ")")),
    " write module to file";
  "-w", Arg.Int (fun n -> Flags.width := n),
    " configure output width (default is 80)";
  "-s", Arg.Set Flags.print_sig, " show module signatures";
  "-u", Arg.Set Flags.unchecked, " unchecked, do not perform validation";
  "-h", Arg.Clear Flags.harness, " exclude harness for JS convesion";
  "-d", Arg.Set Flags.dry, " dry, do not run program";
  "-t", Arg.Set Flags.trace, " trace execution";
  "-v", Arg.Unit banner, " show version"
]
(*IF-OCAML*)
let () =

  Printexc.record_backtrace true;
(*ENDIF-OCAML*)
(*F#
let flush_all () = System.Console.Out.Flush();System.Console.Error.Flush()  //TBR
let go() =
  //TBR
F#*)
  try
    configure ();
    Arg.parse argspec
      (fun file -> add_arg ("(input " ^ quote file ^ ")")) usage;
    List.iter (fun arg -> if not (Run.run_string arg) then exit 1) !args;
    if !args = [] then Flags.interactive := true;
    if !Flags.interactive then begin
      Flags.print_sig := true;
      banner ();
      Run.run_stdin ();
    end
  with exn ->
    flush_all ();
    prerr_endline
(*IF-OCAML*)
    (Sys.argv.(0) ^ ": uncaught exception " ^ Printexc.to_string exn);
    Printexc.print_backtrace stderr;
(*ENDIF-OCAML*)
(*F#
    (System.Environment.GetCommandLineArgs().[0] ^ ": uncaught exception " ^ Printexc.to_string exn);
     System.Console.Error.WriteLine(exn.StackTrace);
    flush_all ();
F#*)
    exit 2

(*F#
[<EntryPoint>]
let main(_:string[]):int =
    go();
    flush_all ();
    0
F#*)