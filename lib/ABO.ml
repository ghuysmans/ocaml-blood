type (_, _) t = int

let bits = 2

let a : ([`A] * [`Nil], [< `A | `Nil] * [`Nil]) t = 1
let b : ([`Nil] * [`B], [`Nil] * [< `B | `Nil]) t = 2
let ab : ([`A] * [`B], [< `A | `Nil] * [< `B | `Nil]) t = 3
let o : ([`Nil] * [`Nil], [< `A | `Nil] * [< `B | `Nil]) t = 0

let compatible ~(donor: ('x, _) t) ~(recipient: (_, 'x) t) =
  lnot donor lor recipient = -1

let to_string : type d r. (d, r) t -> string =
  Array.get [| "O"; "A"; "B"; "AB" |]

let groups =
  [a; b; ab; o] |>
  List.map (fun x -> to_string x, x)
