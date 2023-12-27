module Make (G : Sig.S) = struct
  open G

  type w = W : _ G.t -> w
  (** Untyped wrapper *)

  let of_string s = List.assoc s groups
end
