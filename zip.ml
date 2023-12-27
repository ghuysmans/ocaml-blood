module type S = sig type _ t end

module Zip (A : S) (B : S) : S = struct
  type _ t = P : 'a A.t * 'b B.t -> ('a * 'b) t
end

