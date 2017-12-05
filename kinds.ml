type kind = Star
          | Arrow of kind * kind

let rec str_kind =
  function Star -> "*"
         | (Arrow ((Arrow (k1, k2)), k3)) -> "(" ^ str_kind (Arrow (k1, k2)) ^ ") => " ^ str_kind k3
         | (Arrow (k1, k2)) -> str_kind k1 ^ " => " ^ str_kind k2

let () = let k = Star in
         str_kind k |> print_endline;
         str_kind (Arrow (k, (Arrow (k, k)))) |> print_endline;
         str_kind (Arrow ((Arrow (k, k)), k)) |> print_endline;
         str_kind (Arrow (k, k)) |> print_endline
