(*F#
module Int32 =
struct

    let t = int32
    let sub i j = i - j
    let add i j = i + j
    let max_int = System.Int32.MaxValue
    let min_int = System.Int32.MinValue
    let of_int i = i
    let to_int i = i
end

module Int64 =
struct
    let of_int (i:int32) = (int64) i
    let of_int32 (i:int) = (int64) i
    let to_int (i:int64) = (int) i
end

let max_int = Int32.max_int

let Invalid_argument s = new System.ArgumentException(s) //TBR

module Buffer =
    struct 
          type t = System.Text.StringBuilder
          let  create (n:int) = new System.Text.StringBuilder(n)
          let add_char (b:t) (c:char) = ignore (b.Append(c))
          let contents (b:t) = b.ToString()
   end


F#*)


module Fun =
struct
  let rec repeat n f x =
    if n = 0 then () else (f x; repeat (n - 1) f x)
end

module Int =
struct
  (*IF-OCAML*)
    let log2 n =
    if n <= 0 then failwith "log2";
    let rec loop acc n = if n = 1 then acc else loop (acc + 1) (n lsr 1) in
    loop 0 n

  let is_power_of_two n =
    if n < 0 then failwith "is_power_of_two";
    n <> 0 && n land (n - 1) = 0
   (*ENDIF-OCAML*)
(*F#
    let log2 n =
    if n <= 0 then failwith "log2";
    let rec loop acc n = if n = 1 then acc else loop (acc + 1) (n >>> 1) in //TBR
    loop 0 n

  let is_power_of_two n =
    if n < 0 then failwith "is_power_of_two";
    n <> 0 && n &&& (n - 1) = 0 //TBR
F#*)
end

module String =
struct

   module String  =
   struct
    let sub (s:string) i l = s.Substring(i,l).ToString()
    let index_from (s:string) i (c:char) = s.IndexOf(c,i)
    
   end

  let implode cs =
    let buf = Buffer.create 80 in
    List.iter (Buffer.add_char buf) cs;
    Buffer.contents buf

  let explode s =
    let cs = ref [] in
    for i = String.length s - 1 downto 0 do cs := s.[i] :: !cs done;
    !cs

  let split s c =
    let len = String.length s in
    let rec loop i =
      if i > len then [] else
      let j = try String.index_from s i c with Not_found -> len in
      String.sub s i (j - i) :: loop (j + 1)
    in loop 0

  let breakup s n =
    let rec loop i =
      let len = min n (String.length s - i) in
      if len = 0 then [] else String.sub s i len :: loop (i + len)
    in loop 0

  let sub (s:string) i n =  s.Substring(i,n)
end

module List =
struct
  let rec make n x =
    if n = 0 then [] else x :: make (n - 1) x

  let rec table n f = table' 0 n f
  and table' i n f =
    if i = n then [] else f i :: table' (i + 1) n f

  let rec take n xs =
    match n, xs with
    | 0, _ -> []
    | n, x::xs' when n > 0 -> x :: take (n - 1) xs'
    | _ -> failwith "take"

  let rec drop n xs =
    match n, xs with
    | 0, _ -> xs
    | n, _::xs' when n > 0 -> drop (n - 1) xs'
    | _ -> failwith "drop"

  let rec last = function
    | x::[] -> x
    | _::xs -> last xs
    | [] -> failwith "last"

  let rec split_last = function
    | x::[] -> [], x
    | x::xs -> let ys, y = split_last xs in x::ys, y
    | [] -> failwith "split_last"

  let rec index_where p xs = index_where' p xs 0
  and index_where' p xs i =
    match xs with
    | [] -> None
    | x::xs' when p x -> Some i
    | x::xs' -> index_where' p xs' (i+1)

  let index_of x = index_where ((=) x)

  let rec map_filter f = function
    | [] -> []
    | x::xs ->
      match f x with
      | None -> map_filter f xs
      | Some y -> y :: map_filter f xs
end

module List32 =
struct
  let rec length xs = length' xs 0l
  and length' xs n =
    match xs with
    | [] -> n
    | _::xs' when n < Int32.max_int -> length' xs' (Int32.add n 1l)
    | _ -> failwith "length"

  let rec nth xs n =
    match n, xs with
    | 0l, x::_ -> x
    | n, _::xs' when n > 0l -> nth xs' (Int32.sub n 1l)
    | _ -> failwith "nth"

  let rec take n xs =
    match n, xs with
    | 0l, _ -> []
    | n, x::xs' when n > 0l -> x :: take (Int32.sub n 1l) xs'
    | _ -> failwith "take"

  let rec drop n xs =
    match n, xs with
    | 0l, _ -> xs
    | n, _::xs' when n > 0l -> drop (Int32.sub n 1l) xs'
    | _ -> failwith "drop"
end

module Array32 =
struct

 (*IF-OCAML*)
  let make n x =
    if n < 0l || Int64.of_int32 n > Int64.of_int max_int then
      raise (Invalid_argument "Array32.make");
    Array.make (Int32.to_int n) x
 (*ENDIF-OCAML*)
(*F#
    let make n x =
    if n < 0l || Int64.of_int32 n > Int64.of_int max_int then
      raise (Invalid_argument "Array32.make");
    Array.create (Int32.to_int n) x
F#*)

  let length a = Int32.of_int (Array.length a)

  let index_of_int32 i =
    if i < 0l || Int64.of_int32 i > Int64.of_int max_int then -1 else
    Int32.to_int i

  let get a i = Array.get a (index_of_int32 i)
  let set a i x = Array.set a (index_of_int32 i) x
  let blit a1 i1 a2 i2 n =
    Array.blit a1 (index_of_int32 i1) a2 (index_of_int32 i2) (index_of_int32 n)
end

 (*IF-OCAML*)
module Bigarray =
struct
  open Bigarray

  module Array1_64 =
  struct
    let create kind layout n =
      if n < 0L || n > Int64.of_int max_int then
        raise (Invalid_argument "Bigarray.Array1_64.create");
      Array1.create kind layout (Int64.to_int n)

    let dim a = Int64.of_int (Array1.dim a)

    let index_of_int64 i =
      if i < 0L || i > Int64.of_int max_int then -1 else
      Int64.to_int i

    let get a i = Array1.get a (index_of_int64 i)
    let set a i x = Array1.set a (index_of_int64 i) x
    let sub a i n = Array1.sub a (index_of_int64 i) (index_of_int64 n)
  end
end
 (*ENDIF-OCAML*)
(*F#
module Bigarray =
struct

  module Array1 =
  struct
    type ('a,'b,'c) t  = Array1 of 'a []
    let dim<'a,'b,'c> (Array1 a: ('a,'b,'c) t) = a.Length
    let get (Array1 a) i = a.[i]
    let set (Array1 a) i v = a.[i] <- v
    let sub (Array1 a) i n = Array1 (Array.sub a i n)
  end

  module Array1_64 =
  struct
    let create kind layout n =
      if n < 0L || n > Int64.of_int max_int then
        raise (Invalid_argument "Bigarray.Array1_64.create");
      Array.create (* kind layout *) (Int64.to_int n)

    let dim a = Int64.of_int (Array1.dim a)

    let index_of_int64 i =
      if i < 0L || i > Int64.of_int max_int then -1 else
      Int64.to_int i

    let get a i = Array1.get a (index_of_int64 i)
    let set a i x = Array1.set a (index_of_int64 i) x
    let sub a i n = Array1.sub a (index_of_int64 i) (index_of_int64 n)
  end
end
F#*)




module Option =
struct
  let get o x =
    match o with
    | Some y -> y
    | None -> x

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let app f = function
    | Some x -> f x
    | None -> ()
end
