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

(* TODO try the isomorphism:
module ABO = Pair (Rh) (Rh)
module ABOR = Pair (ABO) (Rh)
*)
module Pair (A : S) (B : S) : sig
  type (_, _) t = P : ('da, 'ra) A.t * ('db, 'rb) B.t -> ('da*'db, 'ra*'rb) t
  include S with type ('d, 'r) t := ('d, 'r) t
end = struct
  type (_, _) t = P : ('da, 'ra) A.t * ('db, 'rb) B.t -> ('da*'db, 'ra*'rb) t

  type c = C : {donor: ('x, _) t; recipient: (_, 'x) t} -> c

  let compatible =
    (* full product *)
    List.concat @@
    List.map (fun (A.C {donor; recipient}) ->
      List.map (fun (B.C {donor=d'; recipient=r'}) ->
        C {donor = P (donor, d'); recipient = P (recipient, r')}
      ) B.compatible
    ) A.compatible

  let to_string : type d r. (d, r) t -> string = fun (P (a, b)) ->
    A.to_string a ^ B.to_string b
end

module G = struct
  include Pair (ABO) (Rh)
  let ap = P (ABO.A, Rh.P) (** A+ *)
  let bp = P (ABO.B, Rh.P) (** B+ *)
  let abp = P (ABO.AB, Rh.P) (** AB+ *)
  let op = P (ABO.O, Rh.P) (** O+ *)
  let an = P (ABO.A, Rh.N) (** A- *)
  let bn = P (ABO.B, Rh.N) (** B- *)
  let abn = P (ABO.AB, Rh.N) (** AB- *)
  let on = P (ABO.O, Rh.N) (** O- *)
end

let dump (module G : S) =
  let open G in
  Printf.printf "digraph {\n";
  List.iter (fun (C {donor; recipient}) ->
    Printf.printf "%S -> %S\n" (to_string donor) (to_string recipient)
  ) compatible;
  Printf.printf "}\n"


let () =
  dump (module G);
