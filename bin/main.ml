open Blood

let () =
  match Sys.argv with
  | [| _; "-g" |] ->
    Dot.output stdout (module ABOR)
  | [| _; donor; recipient |] ->
    let module P = Parser.Make (ABOR) in
    let f s = P.of_string (String.uppercase_ascii s) in
    let donor = f donor in
    let recipient = f recipient in
    print_endline @@
      if ABOR.compatible ~donor ~recipient then
        "compatible"
      else
        "incompatible"
  | _ ->
    Printf.eprintf "usage: %s -g | donor recipient\n" Sys.argv.(0);
    exit 1
