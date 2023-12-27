module ABO = struct
  type ('d_a, 'd_b, 'r_a, 'r_b) t =
    | A : ([`A], [`Nil], [< `A | `Nil], [`Nil]) t
    | B : ([`Nil], [`B], [`Nil], [< `B | `Nil]) t
    | AB : ([`A], [`B], [< `A | `Nil], [< `B | `Nil]) t
    | O : ([`Nil], [`Nil], [< `A | `Nil], [< `B | `Nil]) t

  let compatible (d : ('a, 'b, _, _) t) (r : (_, _, 'a, 'b) t) = ()
end

module Rh = struct
  type ('d, 'r) t =
    | P : ([`D], [< `D | `Nil]) t
    | N : ([`Nil], [`Nil]) t

  let compatible (d : ('a, _) t) (r : (_, 'a) t) = ()
end

type ('d_a, 'd_b, 'd_d, 'r_a, 'r_b, 'r_d) t =
  ('d_a, 'd_b, 'r_a, 'r_b) ABO.t *
  ('d_d, 'r_d) Rh.t

let ap = ABO.A, Rh.P
let bp = ABO.B, Rh.P
let abp = ABO.AB, Rh.P
let op = ABO.O, Rh.P
let an = ABO.A, Rh.N
let bn = ABO.B, Rh.N
let abn = ABO.AB, Rh.N
let on = ABO.O, Rh.N

let compatible (x, y) (x', y') =
  ABO.compatible x x';
  Rh.compatible y y'


let () =
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
