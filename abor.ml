module type S = sig
  type ('donor, 'recipient) t
  type c = C : {donor: ('x, _) t; recipient: (_, 'x) t} -> c
  val compatible : c list
  val to_string : _ t -> string
end

module ABO = struct
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
end

module Rh = struct
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
end

(* FIXME make it compatible with S to allow using the isomorphism:
module ABO = Pair (Rh) (Rh)
module ABOR = Pair (ABO) (Rh)
*)
module Pair (A : S) (B : S) = struct
  type ('d, 'r) t = ('da, 'ra) A.t * ('db, 'rb) B.t
  constraint 'd = 'da * 'db
  constraint 'r = 'ra * 'rb

  type c = C : {donor: ('x, _) t; recipient: (_, 'x) t} -> c

  let compatible =
    (* full product *)
    List.concat @@
    List.map (fun (A.C {donor; recipient}) ->
      List.map (fun (B.C {donor=d'; recipient=r'}) ->
        C {donor = donor, d'; recipient = recipient, r'}
      ) B.compatible
    ) A.compatible

  let to_string : (_, _) t -> string = fun (a, b) ->
    A.to_string a ^ B.to_string b
end

module G = struct
  include Pair (ABO) (Rh)
  let ap = ABO.A, Rh.P (** A+ *)
  let bp = ABO.B, Rh.P (** B+ *)
  let abp = ABO.AB, Rh.P (** AB+ *)
  let op = ABO.O, Rh.P (** O+ *)
  let an = ABO.A, Rh.N (** A- *)
  let bn = ABO.B, Rh.N (** B- *)
  let abn = ABO.AB, Rh.N (** AB- *)
  let on = ABO.O, Rh.N (** O- *)
end

let dump (module G : S) =
  let open G in
  Printf.printf "digraph {\n";
  List.iter (fun (C {donor; recipient}) ->
    Printf.printf "%S -> %S\n" (to_string donor) (to_string recipient)
  ) compatible;
  Printf.printf "}\n"


let () =
  dump (module ABO);
