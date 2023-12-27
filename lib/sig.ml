module type S = sig
  type ('donor, 'recipient) t
  type c = C : {donor: ('x, _) t; recipient: (_, 'x) t} -> c
  val compatible : c list
  val to_string : _ t -> string
end
