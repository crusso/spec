(*IF-OCAML*)
exception Abort of Source.region * string
exception Assert of Source.region * string
exception IO of Source.region * string
(*ENDIF-OCAML*)
(*F#
module Errors = sig
module Abort =
sig
  exception Error of Source.region * string
end
module Assert = 
sig
  exception Error of Source.region * string
end
module IO =
sig
  exception Error of Source.region * string
end
end
exception Abort = Errors.Abort.Error
exception Assert = Errors.Assert.Error
exception IO = Errors.IO.Error 
F#*)



val trace : string -> unit

val run_string : string -> bool
val run_file : string -> bool
val run_stdin : unit -> unit
