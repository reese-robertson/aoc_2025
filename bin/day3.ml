open Aoc_2025
open Base

(* Part 1 *)
let () =
  let ic = Stdio.In_channel.create "puzzle-inputs/day3.txt" in
  let sum =
    In_channel.input_lines ic
    |> List.map ~f:Battery.parse_joltage
    |> List.fold ~init:0 ~f:( + )
  in
  Stdio.printf "Part 1: %d" sum;
  Stdio.print_endline ""
;;
