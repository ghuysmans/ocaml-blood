let output ch (module G : Sig.S) =
  let open G in
  Printf.fprintf ch "digraph {\n";
  List.iter (fun (d, donor) ->
    List.iter (fun (r, recipient) ->
      if compatible ~donor ~recipient then
        Printf.fprintf ch "%S -> %S\n" d r
    ) groups
  ) groups;
  Printf.fprintf ch "}\n"
