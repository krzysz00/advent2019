open Core_kernel
open Core_kernel.In_channel

let operate opcode i1 i2 =
  match opcode with
  | 1 -> i1 + i2
  | 2 -> i1 * i2
  | _ -> invalid_arg "bad opcode"

let rec process arr pos =
  let opcode = arr.(pos)
  in if phys_equal opcode 99 then ()
  else let i1 = arr.(arr.(pos + 1))
    and i2 = arr.(arr.(pos + 2))
    and o = arr.(pos + 3)
    in arr.(o) <- operate opcode i1 i2; process arr (pos + 4); ()

let init input n v =
  let ret = Array.copy input
  in ret.(1) <- n; ret.(2) <- v; ret

let input = input_line stdin |> fun x -> Option.value_exn ~message:"Input failed" x
            |> String.split ~on:','
            |> List.map ~f:int_of_string |> Array.of_list

let run input a b =
  let a = init input a b
  in process a 0; a.(0);;

printf "%d\n" (run input 12 2);;

let target = 19690720

let rec search n v bound =
  if run input n v = target then 100 * n + v
  else if n = bound then -1
  else if v = bound then search (n + 1) 0 bound
  else search n (v + 1) bound;;

printf "%d\n" (search 0 0 100);;
