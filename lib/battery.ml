open Base

let find_highest_digit s ~start =
  let rec doit digit =
    if digit < 0 then raise (Failure "aaaah");
    let digit_c = String.get (Int.to_string digit) 0 in
    let index = String.index_from s start digit_c in
    match index with
    | None -> doit (digit - 1)
    | Some index -> digit, index
  in
  doit 9
;;

let parse_joltage bank =
  let left, index =
    find_highest_digit (String.sub bank ~pos:0 ~len:(String.length bank - 1)) ~start:0
  in
  let right, _ = find_highest_digit bank ~start:(index + 1) in
  (left * 10) + right
;;

let%test_unit "find_highest_digit" =
  [%test_eq: int * int] (find_highest_digit "12345" ~start:0) (5, 4);
  [%test_eq: int * int] (find_highest_digit "90008" ~start:0) (9, 0);
  [%test_eq: int * int] (find_highest_digit "5" ~start:0) (5, 0);
  [%test_eq: int * int] (find_highest_digit "90008" ~start:1) (8, 4)
;;

let%test_unit "parse_joltage" =
  [%test_eq: int] (parse_joltage "55") 55;
  [%test_eq: int] (parse_joltage "009008") 98;
  [%test_eq: int] (parse_joltage "008009") 89;
  [%test_eq: int] (parse_joltage "000007") 7;
  [%test_eq: int] (parse_joltage "818181911112111") 92
;;
