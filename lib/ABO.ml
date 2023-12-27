type (_, _) t =
  | A : ([`A] * [`Nil], [< `A | `Nil] * [`Nil]) t
  | B : ([`Nil] * [`B], [`Nil] * [< `B | `Nil]) t
  | AB : ([`A] * [`B], [< `A | `Nil] * [< `B | `Nil]) t
  | O : ([`Nil] * [`Nil], [< `A | `Nil] * [< `B | `Nil]) t

type c = C : {donor: ('x, _) t; recipient: (_, 'x) t} -> c

let compatible =
  let compatible donor recipient = C {donor; recipient} in
  [
    (* universal donor *)
    compatible O O;
    compatible O A;
    compatible O B;
    (* universal recipient *)
    compatible O AB;
    compatible A AB;
    compatible B AB;
    (* self-compatibility *)
    compatible AB AB;
    compatible A A;
    compatible B B;
  ]

let to_string : type d r. (d, r) t -> string = function
  | A -> "A"
  | B -> "B"
  | AB -> "AB"
  | O -> "O"
