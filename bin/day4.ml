open Aoc_2025
open Base

(* Part 1 *)
let () =
  let ic = Stdio.In_channel.create "puzzle-inputs/day4.txt" in
  let map = In_channel.input_lines ic |> Forklift.parse_freight_map in
  let count =
    List.mapi map ~f:(fun row_index row ->
      List.mapi row ~f:(fun col_index f ->
        match f with
        | Empty -> 0
        | Paper -> if Forklift.is_accessible map ~at:(row_index, col_index) then 1 else 0)
      |> List.fold ~init:0 ~f:( + ))
    |> List.fold ~init:0 ~f:( + )
  in
  Stdio.printf "Part 1: %d" count;
  Stdio.print_endline ""
;;
