module Make (G : Sig.S) = struct
  open G

  type w = W : _ G.t -> w
  (** Untyped wrapper *)

  let compatible' =
    List.map (fun (C {donor=d; recipient=r}) -> W d, W r) compatible
  (** Untyped relation *)

  let groups =
    (* the compatibility relation is reflexive, so we'll get them all *)
    List.map (fun (a, b) -> [a; b]) compatible' |>
    List.concat |>
    List.sort_uniq compare |>
    List.map (fun (W x as w) -> G.to_string x, w)

  let of_string s = List.assoc s groups
end
