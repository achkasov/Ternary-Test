#if INTERACTIVE
module Trit =
#else
module Ternary.Trit
#endif

[<Struct>]
type Trit = Z | N | P

let halfAdd a b =
  match a, b with
  | Z, s | s, Z -> (s, Z)
  | N, P | P, N -> (Z, Z)
  | N, N -> (P, N)
  | P, P -> (N, P)

let addc a b carry =
  let (s1, c1) = halfAdd a b
  let (s2, c2) = halfAdd s1 carry
  let (c3, _ ) = halfAdd c1 c2
  (s2, c3)

let mul a b =
  match a, b with
  | N, P | P, N -> N
  | Z, _ | _, Z -> Z
  | P, P | N, N -> P

let inv a =
  match a with
  | N -> P
  | Z -> Z
  | P -> N

let format a = 
  match a with
  | N -> 'n'
  | Z -> '0'
  | P -> '1'

let toInt a =
  match a with
  | N -> -1
  | Z ->  0
  | P ->  1

let fromInt a =
  match a with
  | a when a < 0 -> N
  | 0 -> Z
  | a when a > 0 -> P
