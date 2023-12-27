module Donor = struct
  type ('a, 'b) t =
    | A : ([`A], [`Nil]) t
    | B : ([`Nil], [`B]) t
    | AB : ([`A], [`B]) t
    | O : ([`Nil], [`Nil]) t
end

module Recipient = struct
  type ('a, 'b) t =
    | A : ([< `A | `Nil], [`Nil]) t
    | B : ([`Nil], [< `B | `Nil]) t
    | AB : ([< `A | `Nil], [< `B | `Nil]) t
    | O : ([< `A | `Nil], [< `B | `Nil]) t
end

let compatible (d : ('a, 'b) Donor.t) (r : ('a, 'b) Recipient.t) = ()


let () =
  compatible Donor.O Recipient.O;
  compatible Donor.O Recipient.A;
  compatible Donor.O Recipient.B;
  compatible Donor.O Recipient.AB;
  compatible Donor.A Recipient.A;
  compatible Donor.A Recipient.AB;
  compatible Donor.B Recipient.B;
  compatible Donor.B Recipient.AB;
  compatible Donor.AB Recipient.AB;
