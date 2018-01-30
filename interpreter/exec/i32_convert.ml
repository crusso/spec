(* WebAssembly-compatible type conversions to i32 implementation *)
(*F#
open FSharp.Compatibility.OCaml
F#*)
let wrap_i64 x = Int64.to_int32 x

let trunc_s_f32 x =
  if F32.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F32.to_float x in
    if xf >= -.(Int32.to_float Int32.min_int) || xf < (Int32.to_float Int32.min_int) then
      raise Numeric_error.IntegerOverflow
    else
      Int32.of_float xf

let trunc_u_f32 x =
  if F32.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F32.to_float x in
    if xf >= -.(Int32.to_float Int32.min_int) *. 2.0 || xf <= -1.0 then
      raise Numeric_error.IntegerOverflow
    else
      (Int64.to_int32 (Int64.of_float xf))

let trunc_s_f64 x =
  if F64.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F64.to_float x in
    if xf >= -.(Int32.to_float Int32.min_int) || xf < (Int32.to_float Int32.min_int) then
      raise Numeric_error.IntegerOverflow
    else
      Int32.of_float xf

let trunc_u_f64 x =
  if F64.ne x x then
    raise Numeric_error.InvalidConversionToInteger
  else
    let xf = F64.to_float x in
    if xf >= -.(Int32.to_float Int32.min_int) *. 2.0 || xf <= -1.0 then
      raise Numeric_error.IntegerOverflow
    else
      Int64.to_int32 (Int64.of_float xf)

let reinterpret_f32 (f:F32.t) = F32.to_bits f
