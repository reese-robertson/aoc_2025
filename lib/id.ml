open Base

(** An id_range is a range of integer IDs in the closed interval [[l, r]]. *)
type id_range = int * int [@@deriving compare, sexp]

let parse_id_range s =
  match String.split s ~on:'-' with
  | [ l; r ] -> Result.Ok (Int.of_string l, Int.of_string r)
  | _ -> Result.Error "invalid range"
;;

let ids_in_range id_range =
  let first_id, last_id = id_range in
  let rec doit id accum =
    let accum = id :: accum in
    if id = last_id then accum else (doit [@tailcall]) (id + 1) accum
  in
  List.rev (doit first_id [])
;;

let split_id_into id ~n =
  let len = String.length id in
  match len % n with
  | 0 ->
    let chunk_len = len / n in
    let rec doit start accum =
      if start = len
      then accum
      else
        (doit [@tailcall])
          (start + chunk_len)
          (String.sub id ~pos:start ~len:chunk_len :: accum)
    in
    Some (doit 0 [] |> List.rev)
  | _ -> None
;;

let split_id id =
  let len = String.length id in
  let middle_index = len / 2 in
  let l = String.sub id ~pos:0 ~len:middle_index in
  let r = String.sub id ~pos:middle_index ~len:middle_index in
  l, r
;;

(** contains_repetition returns true if the given id can be split into num_groups identical, sequential
  groups.
*)
let contains_repetition id ~num_groups =
  let groups = split_id_into id ~n:num_groups in
  match groups with
  | None -> false
  | Some groups ->
    (match groups with
     | [] -> false
     | head :: tail -> List.for_all tail ~f:(fun group -> String.compare group head = 0))
;;

let is_valid_id id =
  let id = Int.to_string id in
  let len = String.length id in
  match len % 2 with
  | 0 ->
    let l, r = split_id id in
    not (String.compare l r = 0)
  | _ -> true
;;

let sum_invalid_ids_in_range r =
  ids_in_range r
  |> List.filter ~f:(fun id -> not (is_valid_id id))
  |> List.fold ~init:0 ~f:( + )
;;

let sum_invalid_ids_in_ranges id_ranges =
  List.map id_ranges ~f:sum_invalid_ids_in_range |> List.fold ~init:0 ~f:( + )
;;

let%test_unit "ids_in_range" =
  [%test_eq: int list] (ids_in_range (1, 5)) [ 1; 2; 3; 4; 5 ];
  [%test_eq: int list] (ids_in_range (-2, 2)) [ -2; -1; 0; 1; 2 ]
;;

let%test_unit "split_id_into" =
  [%test_eq: string list option] (split_id_into "446446" ~n:1) (Some [ "446446" ]);
  [%test_eq: string list option] (split_id_into "446446" ~n:2) (Some [ "446"; "446" ]);
  [%test_eq: string list option] (split_id_into "446446" ~n:3) (Some [ "44"; "64"; "46" ]);
  [%test_eq: string list option] (split_id_into "446446" ~n:4) None;
  [%test_eq: string list option]
    (split_id_into "446446" ~n:6)
    (Some [ "4"; "4"; "6"; "4"; "4"; "6" ]);
  [%test_eq: string list option] (split_id_into "446446" ~n:12) None;
  [%test_eq: string list option]
    (split_id_into "1234512345" ~n:2)
    (Some [ "12345"; "12345" ]);
  [%test_eq: string list option]
    (split_id_into "1234512345" ~n:5)
    (Some [ "12"; "34"; "51"; "23"; "45" ])
;;

let%test_unit "split_id" =
  [%test_eq: string * string] (split_id "22") ("2", "2");
  [%test_eq: string * string] (split_id "12342341") ("1234", "2341");
  [%test_eq: string * string] (split_id "1188511885") ("11885", "11885");
  [%test_eq: string * string] (split_id "446446") ("446", "446")
;;

let%test_unit "contains_repetition" =
  [%test_eq: bool] (contains_repetition "22" ~num_groups:1) true;
  [%test_eq: bool] (contains_repetition "22" ~num_groups:2) true;
  [%test_eq: bool] (contains_repetition "22" ~num_groups:3) false;
  [%test_eq: bool] (contains_repetition "2222" ~num_groups:1) true;
  [%test_eq: bool] (contains_repetition "2222" ~num_groups:2) true;
  [%test_eq: bool] (contains_repetition "2222" ~num_groups:3) false;
  [%test_eq: bool] (contains_repetition "2222" ~num_groups:4) true;
  [%test_eq: bool] (contains_repetition "1234512345" ~num_groups:2) true;
  [%test_eq: bool] (contains_repetition "1234512345" ~num_groups:5) false
;;

let%test_unit "is_valid_id" =
  [%test_eq: bool] (is_valid_id 11) false;
  [%test_eq: bool] (is_valid_id 12) true;
  [%test_eq: bool] (is_valid_id 22) false;
  [%test_eq: bool] (is_valid_id 303) true;
  [%test_eq: bool] (is_valid_id 1188511885) false
;;

let%test_unit "sum_invalid_ids_in_ranges" =
  [%test_eq: int]
    (sum_invalid_ids_in_ranges
       [ 11, 22
       ; 95, 115
       ; 998, 1012
       ; 1188511880, 1188511890
       ; 222220, 222224
       ; 1698522, 1698528
       ; 446443, 446449
       ; 38593856, 38593862
       ; 565653, 565659
       ; 824824821, 824824827
       ; 2121212118, 2121212124
       ])
    1227775554
;;
