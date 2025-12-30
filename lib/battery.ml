open Base

(** index_within returns the index of a character the given string. This differs from String library methods
by optionally constraining the search to range given by start and len. *)
let index_within ?(start = 0) ?len s c =
  let upper_bound =
    match len with
    | None -> String.length s
    | Some len -> start + len
  in
  let index = String.index_from s start c in
  match index with
  | None -> None
  | Some index when index >= upper_bound -> None
  | Some index -> Some index
;;

let ordered_digits = [ '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; '1'; '0' ]

(** find_highest_digit returns the highest digit and its index in a string. The search can be constrained to a
substring with start and len. *)
let find_highest_digit ?(start = 0) ?len s =
  let rec doit digits =
    match digits with
    | [] -> raise (Failure "aaaah")
    | digit :: tail ->
      let index = index_within ~start ?len s digit in
      (match index with
       | None -> (doit [@tailcall]) tail
       | Some index -> Char.get_digit_exn digit, index)
  in
  doit ordered_digits
;;

(** parse_joltage returns the maximum possible joltage of a given battery bank when up to len batteries are used. *)
let parse_joltage bank ~len =
  let rec doit ~start num_digits acc =
    if num_digits = 0
    then acc
    else (
      (* Constrain our search, we need to leave enough to resolve all digits. *)
      let remaining_len = String.length bank - start in
      let allowed_len = remaining_len - (num_digits - 1) in
      let digit, index = find_highest_digit bank ~start ~len:allowed_len in
      (* Base 10 left shift the value so far and add our digit. *)
      let acc = (acc * 10) + digit in
      (doit [@tailcall]) ~start:(index + 1) (num_digits - 1) acc)
  in
  doit ~start:0 len 0
;;

let%test_unit "find_highest_digit" =
  [%test_eq: int * int] (find_highest_digit "12345") (5, 4);
  [%test_eq: int * int] (find_highest_digit "90008") (9, 0);
  [%test_eq: int * int] (find_highest_digit "5") (5, 0);
  [%test_eq: int * int] (find_highest_digit "90008" ~start:1) (8, 4)
;;

let%test_unit "parse_joltage" =
  [%test_eq: int] (parse_joltage "55" ~len:2) 55;
  [%test_eq: int] (parse_joltage "009008" ~len:2) 98;
  [%test_eq: int] (parse_joltage "009008" ~len:5) 9008;
  [%test_eq: int] (parse_joltage "008009" ~len:2) 89;
  [%test_eq: int] (parse_joltage "000007" ~len:2) 7;
  [%test_eq: int] (parse_joltage "818181911112111" ~len:2) 92;
  [%test_eq: int] (parse_joltage "987654321111111" ~len:12) 987654321111;
  [%test_eq: int] (parse_joltage "811111111111119" ~len:12) 811111111119;
  [%test_eq: int] (parse_joltage "234234234234278" ~len:12) 434234234278;
  [%test_eq: int] (parse_joltage "818181911112111" ~len:12) 888911112111
;;
