open Source
open Ast



(*IF-OCAML*)
module Unknown = Error.Make ()
exception Unknown = Unknown.Error  (* indicates unknown import name *)

module Registry = Map.Make(struct type t = Ast.name let compare = compare end)
(*ENDIF-OCAML*)
(*F#
open FSharp.Compatibility.OCaml 
module Errors = struct
module Unknown =
struct
  exception Error of Source.region * string
  let warn at m = prerr_endline (Source.string_of_region at ^ ": warning: " ^ m)
  let error at m = raise (Error (at, m))
end
end
open Errors
exception Unknown = Unknown.Error


module Registry =
   struct
        type 'a t = Map<Ast.name,'a>
        let empty = Map.empty
        let add = Map.add
        let find = Map.find
   end

F#*)






let registry = ref Registry.empty

let register name lookup = registry := Registry.add name lookup !registry

(*IF-OCAML*)
let lookup (m : module_) (im : import) : Instance.extern =
(*ENDIF-OCAML*)
(*F#
let lookup (m : module_) (im : import) : Instance.``extern`` =
F#*)
  let {module_name=module_name; item_name=item_name; idesc=idesc} = im.it in
  let t = import_type m im in
  try Registry.find module_name !registry item_name t with Not_found ->
    Unknown.error im.at
      ("unknown import \"" ^ string_of_name module_name ^
        "\".\"" ^ string_of_name item_name ^ "\"")

let link m = List.map (lookup m) m.it.imports
