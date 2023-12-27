open Blood

module ABO = Pair.Make (Rh) (Rh)
module ABOR = Pair.Make (ABO) (Rh)

let () =
  Dot.output stdout (module ABOR)
