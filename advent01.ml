open Core_kernel
open Core_kernel.In_channel

(* Integer division rounds down *)
let fuel_of_mass n = (n / 3) - 2

let rec total_fuel_go n acc =
  let fuel = fuel_of_mass n
  in if fuel <= 0 then acc else total_fuel_go fuel (acc + fuel)
and total_fuel n = total_fuel_go n 0

let process str = str |> int_of_string |> total_fuel;; (* fuel_of_mass for part 1 *)

let result = fold_lines stdin
             ~init:0 ~f:(fun v s -> process s + v)
in printf "%d\n" result
