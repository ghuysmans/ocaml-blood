module Make (A : Sig.S) (B : Sig.S) = struct
  type (_, _) t = P : ('da, 'ra) A.t * ('db, 'rb) B.t -> ('da*'db, 'ra*'rb) t

  type c = C : {donor: ('x, _) t; recipient: (_, 'x) t} -> c

  let compatible =
    (* full product *)
    List.concat @@
    List.map (fun (A.C {donor; recipient}) ->
      List.map (fun (B.C {donor=d'; recipient=r'}) ->
        C {donor = P (donor, d'); recipient = P (recipient, r')}
      ) B.compatible
    ) A.compatible

  let to_string : type d r. (d, r) t -> string = fun (P (a, b)) ->
    A.to_string a ^ B.to_string b
end
