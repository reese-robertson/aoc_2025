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

let pow base p =
  let rec doit base p accum =
    if p = 0 then accum else (doit [@tailcall]) base (p - 1) (base * accum)
  in
  doit base p 1
;;

let slow_log_base_10 num =
  let rec doit num accum =
    if num < 10 then accum else (doit [@tailcall]) (num / 10) (accum + 1)
  in
  doit num 0
;;

let split_id id =
  (* Int floor of log_10 + 1 gives us the number of digits of a base 10 number. *)
  let sig_fig = slow_log_base_10 id + 1 in
  let shift = sig_fig / 2 in
  let div = pow 10 shift in
  (* Right shift the upper by dividing by some power of 10. *)
  let l = id / div in
  (* Isolate the lower half by modulo the same power of 10, zeroing the upper digits. *)
  let r = id % div in
  l, r
;;

let is_valid_id id =
  let l, r = split_id id in
  not (l = r)
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

let%test_unit "pow" =
  [%test_eq: int] (pow 2 2) 4;
  [%test_eq: int] (pow 5 2) 25;
  [%test_eq: int] (pow 5 1) 5;
  [%test_eq: int] (pow 7 0) 1;
  [%test_eq: int] (pow 10 3) 1000
;;

let%test_unit "slow_log_base_10" =
  [%test_eq: int] (slow_log_base_10 100) 2;
  [%test_eq: int] (slow_log_base_10 303) 2;
  [%test_eq: int] (slow_log_base_10 1000) 3;
  [%test_eq: int] (slow_log_base_10 1258) 3
;;

let%test_unit "split_id" =
  [%test_eq: int * int] (split_id 22) (2, 2);
  [%test_eq: int * int] (split_id 12341234) (1234, 1234);
  [%test_eq: int * int] (split_id 12342341) (1234, 2341);
  [%test_eq: int * int] (split_id 303) (30, 3);
  [%test_eq: int * int] (split_id 1188511885) (11885, 11885);
  [%test_eq: int * int] (split_id 446446) (446, 446)
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
