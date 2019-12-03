open ExtString

let operate opcode i1 i2 =
  match opcode with
  | 1 -> i1 + i2
  | 2 -> i1 * i2
  | _ -> invalid_arg "bad opcode"

let rec process arr pos =
  let opcode = arr.(pos)
  in if opcode == 99 then ()
  else let i1 = arr.(arr.(pos + 1))
    and i2 = arr.(arr.(pos + 2))
    and o = arr.(pos + 3)
    in arr.(o) <- operate opcode i1 i2; process arr (pos + 4); ()

let init input n v =
  let ret = Array.copy input
  in ret.(1) <- n; ret.(2) <- v; ret

let input = input_line stdin |> String.split_on_char ','
            |> List.map int_of_string |> Array.of_list

let debug a = a |> Array.map string_of_int |> Array.to_list |> String.concat " " |> print_endline; a

let run input a b =
  let a = init input a b
  in process a 0; a.(0);;

print_int (run input 12 2); print_newline ();;

let target = 19690720

let rec search n v bound =
  if run input n v = target then 100 * n + v
  else if n == bound then -1
  else if v == bound then search (n + 1) 0 bound
  else search n (v + 1) bound;;

print_int (search 0 0 100) ; print_newline ();;
