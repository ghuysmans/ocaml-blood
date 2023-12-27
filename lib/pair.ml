module Make (A : Sig.S) (B : Sig.S) = struct
  type (_, _) t = int

  let bits = A.bits + B.bits

  let cons a b = a lsl B.bits lor b

  let unpack x =
    let mask n = 1 lsl n - 1 in
    x lsr B.bits land mask A.bits,
    x land mask B.bits

  let compatible ~(donor: ('x, _) t) ~(recipient: (_, 'x) t) =
    let da, db = unpack donor in
    let ra, rb = unpack recipient in
    A.compatible ~donor:da ~recipient:ra &&
    B.compatible ~donor:db ~recipient:rb

  let to_string : type d r. (d, r) t -> string = fun x ->
    let a, b = unpack x in
    A.to_string a ^ B.to_string b

  let groups =
    (* full product *)
    List.concat @@
    List.map (fun (an, a) ->
      List.map (fun (bn, b) ->
        an ^ bn, cons a b
      ) B.groups
    ) A.groups
end
