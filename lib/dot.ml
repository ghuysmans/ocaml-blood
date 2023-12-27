let output ch (module G : Sig.S) =
  let open G in
  Printf.fprintf ch "digraph {\n";
  List.iter (fun (C {donor; recipient}) ->
    Printf.fprintf ch "%S -> %S\n" (to_string donor) (to_string recipient)
  ) compatible;
  Printf.fprintf ch "}\n"
