module Make (A : Sig.S) (B : Sig.S) : sig
  type (_, _) t = P : ('da, 'ra) A.t * ('db, 'rb) B.t -> ('da*'db, 'ra*'rb) t
  include Sig.S with type ('d, 'r) t := ('d, 'r) t
end
