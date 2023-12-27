include Pair.Make (ABO) (Rh)

let ap = cons ABO.a Rh.p
(** A+ *)

let bp = cons ABO.b Rh.p
(** B+ *)

let abp = cons ABO.ab Rh.p
(** AB+ *)

let op = cons ABO.o Rh.p
(** O+ *)

let an = cons ABO.a Rh.n
(** A- *)

let bn = cons ABO.b Rh.n
(** B- *)

let abn = cons ABO.ab Rh.n
(** AB- *)

let on = cons ABO.o Rh.n
(** O- *)
