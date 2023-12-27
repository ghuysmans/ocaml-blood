type ('d_a, 'd_b, 'r_a, 'r_b) t =
  | A : ([`A], [`Nil], [< `A | `Nil], [`Nil]) t
  | B : ([`Nil], [`B], [`Nil], [< `B | `Nil]) t
  | AB : ([`A], [`B], [< `A | `Nil], [< `B | `Nil]) t
  | O : ([`Nil], [`Nil], [< `A | `Nil], [< `B | `Nil]) t

let compatible (d : ('a, 'b, _, _) t) (r : (_, _, 'a, 'b) t) = ()


let () =
  compatible O O;
  compatible O A;
  compatible O B;
  compatible O AB;
  compatible A A;
  compatible A AB;
  compatible B B;
  compatible B AB;
  compatible AB AB;
