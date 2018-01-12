module Ternary.Main

open Ternary.Trit

[<EntryPoint>]
let main argv =
  let a = [| N; N; N; |]
  let b = [| Z; Z; Z; |]
  let sum =
    Tryte.add a b
    |> Tryte.normalise
    |> Tryte.resize 9u
    |> Tryte.toInt
    //|> Tryte.format3

  let x = [|N; N; Z; Z; N; Z; P; N; Z; N|]

  0