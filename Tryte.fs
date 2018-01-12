#if INTERACTIVE
module Tryte =
open Trit
#else
module Ternary.Tryte
#endif

type Tryte = Trit.Trit[]
let alphabet = [| 'v'; 'c'; 'x'; 'z'; 'l'; 'k'; 'j'; 'h'; 'g'; 'f'; 'd'; 's'; 'a';
                  '0';
                  '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'Q'; 'W'; 'E'; 'R'; |]

let inline normalise (a: Tryte) : Tryte =
  let inline trimInitialZs arr elem =
    match arr, elem with
    | [||], Trit.Z -> [| Trit.Z |]
    | [|Trit.Z|], Trit.Z -> [| Trit.Z |]
    | _, _ -> Array.append arr [| elem |]
  match a with
  | [||] -> [| Trit.Z |]
  | _ -> a |> Array.rev |> Array.fold trimInitialZs [||] |> Array.rev

let inline resize (size: uint32) (a: Tryte) : Tryte =
  let result = Array.zeroCreate<Trit.Trit> (int size)
  let copyLength = min (a.Length) (int size)
  Array.blit a 0 result 0 copyLength
  result

let split (groupSize: uint32) (a: Tryte) : Tryte[] =
  let size = match groupSize with 0u -> a.Length | n -> int n
  a
  |> Array.mapi (fun index trit -> index / size, trit)
  |> Array.groupBy fst
  |> Array.map ((fun (_, group) -> group |> Array.map snd) >> resize (uint32 size) )

let add (a: Tryte) (b: Tryte) : Tryte =
  match a, b with
  | [||], [||] -> [||]
  | _, _ ->
    let maxLen = max a.Length b.Length
    let a' = resize (uint32 maxLen) a
    let b' = resize (uint32 maxLen) b
    let mutable car = [| for _ in 0 .. maxLen -> Trit.Z |]
    let mutable sum = car
    for i in 0 .. (maxLen - 1) do
      let (s, c) = Trit.addc a'.[i] b'.[i] car.[i]
      car.[i+1] <- c
      sum.[i] <- s
    sum

let toInt (a: Tryte) : int =
  a
  |> function [||] -> [| Trit.Z |]  | arr  -> arr
  |> normalise
  |> Array.mapi (fun i trit -> (3.0 ** float i |> int) * (Trit.toInt trit))
  |> Array.sum

let inline inc (a: Tryte) : Tryte = add a [| Trit.P |]
let inline dec (a: Tryte) : Tryte = add a [| Trit.Z |]
let inline inv (a: Tryte) : Tryte = a |> Array.map (Trit.inv)
let inline shl (s: uint32) (a: Tryte) : Tryte = a |> normalise |> Array.append (Array.zeroCreate<Trit.Trit> (int s)) |> normalise
let inline shr (s: uint32) (a: Tryte) : Tryte = a |> normalise |> Array.skip (min (int s) (a.Length)) |> normalise

let inline shf (s: Tryte) (a: Tryte) : Tryte =
  match toInt s with
  | n when n < 0 -> shr (-n |> uint32) a
  | 0 -> a |> normalise
  | n when n > 0 -> shl (n |> uint32) a

let inline fromTrit (trit: Trit.Trit) : Tryte = [| trit |]

let fromInt (a: int) : Tryte =
  let inline adjustRemainder r = 
    match r with
    | -2 -> (  1, -1 )
    |  2 -> ( -1,  1 )
    |  a -> (  a,  0 )

  let rec convert (x: int) (tryte: Tryte) : Tryte =
    match x with
    | -1 -> Array.append tryte [| Trit.N |]
    |  0 -> Array.append tryte [| Trit.Z |]
    |  1 -> Array.append tryte [| Trit.P |]
    |  x -> 
      let r, c = adjustRemainder (x % 3)
      convert (x / 3 + c) (Array.append tryte [| Trit.fromInt r |])

  convert a [||] |> normalise

let format3 (tryte: Tryte) : string =
  tryte
  |> Array.rev
  |> Array.map (Trit.format)
  |> System.String

let format27 (tryte: Tryte) : string = 
  let inline tripletToChar triplet = alphabet.[ toInt triplet + 13 ]
  let targetLen = ceil ((float tryte.Length) / 3.0) * 3.0 |> uint32
  tryte
  |> resize targetLen
  |> split 3u
  |> Array.map tripletToChar
  |> Array.rev
  |> System.String

let tryParse (s: string) : Tryte option =
  let inline charToInt c = Array.tryFindIndex (fun a -> System.Char.ToUpper a = c) alphabet
  let intArray =
    s.ToUpper().ToCharArray()
    |> Array.rev
    |> Array.map charToInt
    |> Array.fold (fun acc elem ->
      match acc, elem with
      | None, _ -> None
      | _, None -> None
      | Some(arr), Some(x) -> Some( Array.append arr [| x - 13 |] )
      ) (Some [||])

  match intArray with
  | None -> None
  | Some(arr) -> 
    arr
    |> Array.mapi (fun i x -> (27.0 ** (float i)) * (float x) |> int)
    |> Array.sum
    |> fromInt
    |> Some

let mul (a: Tryte) (b: Tryte) : Tryte = fromInt (toInt a * toInt b)