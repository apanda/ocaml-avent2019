open Core

let get_filename args = args.(1)

(* Only need one line in this case *)
let read_line = In_channel.read_all

(* Split on line *)
let split = String.split ~on:','

exception ExecutionError of string

let rec execute index arr =
  match arr.(index) with
  | 1 ->
    arr.(arr.(index + 3)) <- arr.(arr.(index + 1)) + arr.(arr.(index + 2));
    execute (index + 4) arr
  | 2 ->
    arr.(arr.(index + 3)) <- arr.(arr.(index + 1)) * arr.(arr.(index + 2));
    execute (index + 4) arr
  | 99 -> arr
  | _ -> raise (ExecutionError "Unknown opcode")
;;

let prepare_execute n v arr =
  arr.(1) <- n;
  arr.(2) <- v;
  let _ = execute 0 arr in
  arr.(0)
;;

let hunt_noun_verb arr =
  let rec hunt_noun n arr =
    if n > 99
    then 11000
    else (
      let rec hunt_verb v arr =
        if v > 99
        then v
        else if prepare_execute n v (Array.copy arr) = 19690720
        then v
        else hunt_verb (v + 1) arr
      in
      let v = hunt_verb 0 arr in
      if v <= 99 then (100 * n) + v else hunt_noun (n + 1) arr)
  in
  hunt_noun 0 arr
;;

let () =
  Sys.get_argv ()
  |> get_filename
  |> read_line
  |> String.strip
  |> split
  |> List.map ~f:int_of_string
  |> Array.of_list
  |> hunt_noun_verb
  |> string_of_int
  |> print_endline
;;
