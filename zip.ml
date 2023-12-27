module type S = sig type _ t end

module Zip (A : S) (B : S) : S = struct
  type 'p t = 'a A.t * 'b B.t
  constraint 'p = 'a * 'b
end

