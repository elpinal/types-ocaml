(* TAPL; Chapter 20 *)

type stream = Stream of (unit -> int * stream)

let hd (Stream s) = s () |> fst

let tl (Stream s) = s () |> snd

let rec upfrom n = Stream (fun () -> (n, upfrom (n + 1)))

let () = upfrom 10 |> tl |> tl |> hd |> print_int;
         print_newline ()

let rec fib (m, n) = Stream (fun () -> (m, fib (n, m + n)))

let () = let s = fib (1, 1) in
         let s = s |> tl |> tl |> tl |> tl |> tl in
         let n = hd s in
         print_int n;
         print_newline ()
