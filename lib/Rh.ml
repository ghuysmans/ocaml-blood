type (_, _) t =
  | P : ([`D], [< `D | `Nil]) t (** + *)
  | N : ([`Nil], [`Nil]) t (** - *)

type c = C : {donor: ('x, _) t; recipient: (_, 'x) t} -> c

let compatible =
  let compatible donor recipient = C {donor; recipient} in
  [
    compatible P P;
    compatible N P;
    compatible N N;
  ]

let to_string : type d r. (d, r) t -> string = function
  | P -> "+"
  | N -> "-"
