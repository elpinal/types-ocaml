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

type ty = Var of string
        | Con of string
        | App of ty * ty

let rec str_ty =
  function Var i -> i
         | Con i -> i
         | App (t1, (App (t2, t3) as a)) -> str_ty t1 ^ " (" ^ str_ty a ^ ")"
         | App (t1, t2) -> str_ty t1 ^ " " ^ str_ty t2

let () = let tv = Var "a" in
         let tc = Con "A" in
         str_ty tv |> print_endline;
         str_ty (App (tc, (App (tc, tv)))) |> print_endline;
