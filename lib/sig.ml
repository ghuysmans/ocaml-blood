module type S = sig
  type ('donor, 'recipient) t = private int
  val bits : int
  val compatible : donor: ('x, _) t -> recipient: (_, 'x) t -> bool
  val to_string : _ t -> string
  val groups : (string * int) list
end
