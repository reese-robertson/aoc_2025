open Aoc_2025

exception Bad_input of string

let must_ok r =
  match r with
  | Result.Ok v -> v
  | Result.Error v -> raise (Bad_input v)
;;

(* Part 1 *)
let () =
  let ic = open_in "puzzle-inputs/day1.txt" in
  let turns =
    In_channel.input_lines ic |> List.map Dial.parse_dial_turn |> List.map must_ok
  in
  let positions = Dial.perform_turns turns 50 in
  let c = List.length @@ List.filter (fun p -> p = 0) positions in
  Printf.printf "Part 1: %d" c;
  print_newline ()
;;

(* Part 2 *)
let () =
  let ic = open_in "puzzle-inputs/day1.txt" in
  let turns =
    In_channel.input_lines ic |> List.map Dial.parse_dial_turn |> List.map must_ok
  in
  let c = Dial.count_passes turns 50 in
  Printf.printf "Part 2: %d" c;
  print_newline ()
;;
