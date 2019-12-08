open Core_kernel
open Core_kernel.In_channel

let get_line = fun _x ->
  Option.value_exn ~message:"Input failed" (input_line stdin)

type move_dir = Right | Left | Up | Down
let move_dir_of_char c =
  match c with
  | 'R' -> Right
  | 'L' -> Left
  | 'U' -> Up
  | 'D' -> Down
  | x -> invalid_arg (sprintf "Unknown direction: %c" x)

type move = move_dir * int
let move_of_string str =
  let dir = move_dir_of_char str.[0]
  and length = String.drop_prefix str 1 |> int_of_string
  in (dir, length)

let move_point (x, y) (dir, length) =
  let xscale = match dir with
    | Right -> 1 | Left -> -1 | Up -> 0 | Down -> 0
  and yscale = match dir with
    | Right -> 0 | Left -> 0 | Up -> 1 | Down -> -1
  in (x + xscale * length, y + yscale * length)


let parse_line line = String.split ~on:',' line
                      |> List.map ~f:move_of_string

type segment_dir = Horiz | Vert
type segment = { dir: segment_dir; const: int; start: int; stop: int }

let make_segment (x, y)  (dir, length) =
  let (xf, yf) as stop = move_point (x, y) (dir, length)
  and dir = match dir with | Left | Right -> Horiz | Up | Down -> Vert
  in let segment = match dir with
      | Horiz -> { start = x; stop = xf; const = y; dir = dir }
      | Vert -> { start = y; stop = yf; const = x; dir = dir }
  in (segment, stop)


let path_fold (accum, point) move =
  let (segment, new_point) = make_segment point move
  in (segment :: accum, new_point)

let build_path moves =
  moves |> List.fold ~init:([], (0, 0)) ~f:path_fold |> fst |> List.rev

let between x a b =
  if a <= b then a <= x && x <= b else b <= x && x <= a

let intersect_hv x y =
  match x, y with
  | { dir = Horiz; const = cy; start = xs; stop = xf },
    { dir = Vert; const = cx; start = ys; stop = yf } ->
    if (between cx xs xf && between cy ys yf)
    then Some (cx, cy)
    else None
  | x -> invalid_arg "Invalid argument"

let intersect a b =
  match a.dir, b.dir with
      | Horiz, Horiz | Vert, Vert -> None
      | Horiz, Vert -> intersect_hv a b
      | Vert, Horiz -> intersect_hv b a

let manhattan_norm (x, y) = abs x + abs y

let solve: segment list -> segment list -> ((int * int) -> int) -> (int * int) option =
  let open List in
  fun l1 l2 metric ->
  let intersects = l1 >>= (fun a -> filter_map l2 (fun b -> intersect a b))
  in min_elt ~compare:(fun x y -> Int.compare (metric x) (metric y))
    intersects

let show_solution soln metric =
    match soln with
    | Some ((x, y) as point) -> printf "(%d, %d): %d\n" x y (metric point)
    | None -> printf "No intersection\n"

let l1 = get_line () |> parse_line |> build_path
let l2 = get_line () |> parse_line |> build_path

let program metric =
  let soln = solve l1 l2 metric
  in show_solution soln metric

let () = program manhattan_norm;;
