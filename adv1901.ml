open Core

let get_filename args = args.(1)
let read_lines = In_channel.read_lines

let rec mass_to_fuel m =
  let f = (m / 3) - 2 in
  if f <= 0 then 0 else f + mass_to_fuel f
;;

let () =
  let out =
    Sys.get_argv ()
    |> get_filename
    |> read_lines
    |> List.map ~f:int_of_string
    |> List.map ~f:mass_to_fuel
    |> List.reduce ~f:( + )
    |> Option.map ~f:string_of_int
    |> Option.map ~f:print_endline
  in
  Option.value_exn out
;;
