type (_, _) t = int

let bits = 1

let p : ([`D], [< `D | `Nil]) t = 1
(** + *)

let n : ([`Nil], [`Nil]) t = 0
(** - *)

let compatible ~(donor: ('x, _) t) ~(recipient: (_, 'x) t) =
  lnot donor lor recipient = -1

let to_string : type d r. (d, r) t -> string =
  Array.get [| "-"; "+" |]

let groups =
  [p; n] |>
  List.map (fun x -> to_string x, x)
