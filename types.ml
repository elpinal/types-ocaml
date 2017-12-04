(* TAPL; Chapter 20 *)

type stream = Stream of (unit -> int * stream)

let hd (Stream s) = s () |> fst

let tl (Stream s) = s () |> snd

let rec upfrom n = Stream (fun () -> (n, upfrom (n + 1)))

let () = upfrom 10 |> tl |> tl |> hd |> print_int;
         print_newline ()
