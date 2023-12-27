include Pair.Make (ABO) (Rh)

let ap = P (ABO.A, Rh.P)
(** A+ *)

let bp = P (ABO.B, Rh.P)
(** B+ *)

let abp = P (ABO.AB, Rh.P)
(** AB+ *)

let op = P (ABO.O, Rh.P)
(** O+ *)

let an = P (ABO.A, Rh.N)
(** A- *)

let bn = P (ABO.B, Rh.N)
(** B- *)

let abn = P (ABO.AB, Rh.N)
(** AB- *)

let on = P (ABO.O, Rh.N)
(** O- *)
