module Make (A : Sig.S) (B : Sig.S) : sig
  include Sig.S
  val cons : ('da, 'ra) A.t -> ('db, 'rb) B.t -> ('da*'db, 'ra*'rb) t
end
