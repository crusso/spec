(*F#
open FSharp.Compatibility.OCaml
F#*)
open Types

type module_inst =
{
  types : func_type list;
  funcs : func_inst list;
  tables : table_inst list;
  memories : memory_inst list;
  globals : global_inst list;
  exports : export_inst list;
}

and func_inst = module_inst ref Func.t
and table_inst = Table.t
and memory_inst = Memory.t
and global_inst = Global.t



(*IF-OCAML*)
and export_inst = Ast.name * extern

and extern =
(*ENDIF-OCAML*)
(*F#
and export_inst = Ast.name * ``extern``

and ``extern`` =
F#*)
  | ExternFunc of func_inst
  | ExternTable of table_inst
  | ExternMemory of memory_inst
  | ExternGlobal of global_inst
(*IF-OCAML*)
type Table.elem += FuncElem of func_inst
(*ENDIF-OCAML*)
(*F#
type FuncElem = FuncElem of func_inst
let (|FuncElem|_|) (o:Table.elem) = match o with 
      | :? FuncElem as funcelem  -> let (FuncElem fi) = funcelem in Some fi
      | _ -> None
let FuncElem fi = (FuncElem fi) :> Table.elem
F#*)
(* Auxiliary functions *)

let empty_module_inst =
  { types = []; funcs = []; tables = []; memories = []; globals = [];
    exports = [] }

let extern_type_of = function
  | ExternFunc func -> ExternFuncType (Func.type_of func)
  | ExternTable tab -> ExternTableType (Table.type_of tab)
  | ExternMemory mem -> ExternMemoryType (Memory.type_of mem)
  | ExternGlobal glob -> ExternGlobalType (Global.type_of glob)

let export inst name =
  try Some (List.assoc name inst.exports) with Not_found -> None
