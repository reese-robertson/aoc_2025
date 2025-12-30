open Base

type freight =
  | Paper
  | Empty
[@@deriving compare, sexp]

type point = int * int [@@deriving compare, sexp]
type freight_map = freight list list [@@deriving compare, sexp]

let parse_freight c =
  match c with
  | '@' -> Paper
  | '.' -> Empty
  | _ -> raise (Failure "unrecognized freight")
;;

let parse_freight_map lines =
  let parse_line line =
    String.to_sequence line |> Sequence.map ~f:parse_freight |> Sequence.to_list
  in
  List.filter lines ~f:(fun line -> not (String.is_empty line)) |> List.map ~f:parse_line
;;

let get m ~at =
  let open Option.Let_syntax in
  let row_index, col_index = at in
  let%bind row = List.nth m row_index in
  let%bind res = List.nth row col_index in
  return res
;;

let get_adjacent p =
  let x, y = p in
  [ x - 1, y - 1
  ; x - 1, y
  ; x - 1, y + 1
  ; x, y - 1
  ; x, y + 1
  ; x + 1, y - 1
  ; x + 1, y
  ; x + 1, y + 1
  ]
;;

let is_empty m ~at =
  (* Out of bounds points are considered empty. *)
  get m ~at
  |> Option.value_map ~default:true ~f:(fun f ->
    match f with
    | Empty -> true
    | Paper -> false)
;;

let is_accessible m ~at =
  let adjacents = get_adjacent at in
  let num_occupied =
    List.filter adjacents ~f:(fun pos -> not (is_empty m ~at:pos)) |> List.length
  in
  num_occupied < 4
;;

let%test_unit "parse_freight_map" =
  [%test_eq: freight_map]
    (parse_freight_map [ "@.."; ".@." ])
    [ [ Paper; Empty; Empty ]; [ Empty; Paper; Empty ] ];
  [%test_eq: freight_map]
    (parse_freight_map [ "..."; "..." ])
    [ [ Empty; Empty; Empty ]; [ Empty; Empty; Empty ] ]
;;

let _example_freight_map_input =
  {|
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
|}
;;

let _example_freight_map =
  parse_freight_map @@ String.split_lines _example_freight_map_input
;;

let%test_unit "get" =
  [%test_eq: freight option] (get _example_freight_map ~at:(0, 0)) (Some Empty);
  [%test_eq: freight option] (get _example_freight_map ~at:(1, 3)) (Some Empty);
  [%test_eq: freight option] (get _example_freight_map ~at:(3, 0)) (Some Paper);
  [%test_eq: freight option] (get _example_freight_map ~at:(-1, 0)) None
;;

let%test_unit "is_accessible" =
  [%test_eq: bool] (is_accessible _example_freight_map ~at:(0, 2)) true;
  [%test_eq: bool] (is_accessible _example_freight_map ~at:(0, 3)) true;
  [%test_eq: bool] (is_accessible _example_freight_map ~at:(1, 1)) false
;;
