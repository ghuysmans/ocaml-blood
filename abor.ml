module ABO = struct
  type ('d_a, 'd_b, 'r_a, 'r_b) t =
    | A : ([`A], [`Nil], [< `A | `Nil], [`Nil]) t
    | B : ([`Nil], [`B], [`Nil], [< `B | `Nil]) t
    | AB : ([`A], [`B], [< `A | `Nil], [< `B | `Nil]) t
    | O : ([`Nil], [`Nil], [< `A | `Nil], [< `B | `Nil]) t

  type c =
    C : {donor:     ('a, 'b, _, _) t;
         recipient: (_, _, 'a, 'b) t} -> c

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

  let to_string : type a b c d. (a, b, c, d) t -> string = function
    | A -> "A"
    | B -> "B"
    | AB -> "AB"
    | O -> "O"
end

module Rh = struct
  type ('d, 'r) t =
    | P : ([`D], [< `D | `Nil]) t
    | N : ([`Nil], [`Nil]) t

  type c =
    C : {donor:     ('a, _) t;
         recipient: (_, 'a) t} -> c

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
end

type ('d_a, 'd_b, 'd_d, 'r_a, 'r_b, 'r_d) t =
  ('d_a, 'd_b, 'r_a, 'r_b) ABO.t *
  ('d_d, 'r_d) Rh.t

let to_string : type a b c d e f. (a, b, c, d, e, f) t -> string = fun (x, y) ->
  ABO.to_string x ^ Rh.to_string y

let ap = ABO.A, Rh.P
let bp = ABO.B, Rh.P
let abp = ABO.AB, Rh.P
let op = ABO.O, Rh.P
let an = ABO.A, Rh.N
let bn = ABO.B, Rh.N
let abn = ABO.AB, Rh.N
let on = ABO.O, Rh.N

type c =
  C : {donor:     ('a, 'b, 'd, _, _, _) t;
       recipient: (_, _, _, 'a, 'b, 'd) t} -> c

let compatible =
  let compatible donor recipient = C {donor; recipient} in
  [
    (* universal donor *)
    compatible on on;
    compatible on an;
    compatible on bn;
    (* universal recipient *)
    compatible on abn;
    compatible an abn;
    compatible bn abn;
    compatible abn abn;
    (* meh *)
    compatible an an;
    compatible bn bn;
    (* same for Rh *)
    compatible op op;
    compatible op ap;
    compatible op bp;
    compatible op abp;
    compatible ap ap;
    compatible ap abp;
    compatible bp bp;
    compatible bp abp;
    compatible abp abp;
    (* Rh- -> Rh+ *)
    compatible on op;
    compatible on ap;
    compatible on bp;
    compatible on abp;
    compatible an ap;
    compatible an abp;
    compatible bn bp;
    compatible bn abp;
    compatible abn abp;
  ]
