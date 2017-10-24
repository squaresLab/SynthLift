open Bap.Std

type kind = [`Var | `Int]

type t = kind -> exp
