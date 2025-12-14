open Base

(** A representation of turning a dial **)
type dial_turn =
  | Left of int
  | Right of int
[@@deriving compare, sexp]

type t = (dial_turn, string) Result.t [@@deriving sexp]

let parse_dial_turn line =
  let d = String.get line 0 in
  let n_str = String.sub line ~pos:1 ~len:(String.length line - 1) in
  let n = Int.of_string n_str in
  match d with
  | 'L' -> Result.Ok (Left n)
  | 'R' -> Result.Ok (Right n)
  | _ -> Result.Error "invalid direction"
;;

let turn_dial : int -> dial_turn -> int =
  fun start_position next ->
  let new_position =
    match next with
    | Left x -> start_position - x
    | Right x -> start_position + x
  in
  Int.abs (new_position % 100)
;;

let perform_turns turns start_pos =
  let rec doit pos turns accum =
    match turns with
    | [] -> pos :: accum
    | head :: tail -> (doit [@tailcall]) (turn_dial pos head) tail (pos :: accum)
  in
  List.rev (doit start_pos turns [])
;;

let%test_unit "turn_dial" =
  (* Test barely wrapping *)
  [%test_eq: int] (turn_dial 99 (Right 1)) 0;
  [%test_eq: int] (turn_dial 0 (Left 1)) 99;
  [%test_eq: int] (turn_dial 99 (Right 5)) 4;
  [%test_eq: int] (turn_dial 99 (Left 5)) 94;
  [%test_eq: int] (turn_dial 2 (Left 5)) 97;
  [%test_eq: int] (turn_dial 2 (Right 42)) 44;
  (* Test wrapping identity *)
  [%test_eq: int] (turn_dial 42 (Right 100)) 42;
  [%test_eq: int] (turn_dial 42 (Left 100)) 42;
  (* Test multiple wraparounds *)
  [%test_eq: int] (turn_dial 2 (Left 200)) 2;
  [%test_eq: int] (turn_dial 2 (Right 200)) 2
;;

let%test_unit "parse_dial_turn" =
  [%test_eq: (dial_turn, string) Result.t] (parse_dial_turn "L45") (Ok (Left 45));
  [%test_eq: (dial_turn, string) Result.t] (parse_dial_turn "R123") (Ok (Right 123));
  [%test_eq: (dial_turn, string) Result.t]
    (parse_dial_turn "x123")
    (Error "invalid direction")
;;
