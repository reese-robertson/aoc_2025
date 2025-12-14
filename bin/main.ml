open Aoc_2025

exception Bad_input of string

let must_ok r =
  match r with
  | Result.Ok v -> v
  | Result.Error v -> raise (Bad_input v)
;;

let () =
  let ic = open_in "puzzle-inputs/day1.txt" in
  let turns =
    In_channel.input_lines ic |> List.map Dial.parse_dial_turn |> List.map must_ok
  in
  let positions = Dial.perform_turns turns 50 in
  let c = List.filter (fun x -> x = 0) positions in
  Printf.printf "Answer: %d" (List.length c)
;;
