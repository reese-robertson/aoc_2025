open Aoc_2025
open Base

exception Bad_input of string

(* Part 1 *)
let () =
  let ic = Stdio.In_channel.create "puzzle-inputs/day2.txt" in
  let line =
    match In_channel.input_line ic with
    | None -> raise (Bad_input "unexpected empty input")
    | Some line -> line
  in
  let sum =
    String.split ~on:',' line
    |> List.map ~f:Id.parse_id_range
    |> List.map ~f:Result.ok_or_failwith
    |> Id.sum_invalid_ids_in_ranges
  in
  Stdio.printf "Part 1: %d" sum
;;
