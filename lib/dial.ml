open Base

(** A representation of turning a dial *)
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

(** count_passes returns the number of times the dial would pass 0 while performing the given turns. *)
let count_passes turns start_pos =
  let count pos turn =
    match turn with
    | None -> 0
    | Some turn ->
      let start_pos = pos in
      let end_pos = turn_dial start_pos turn in
      (* How many times it fully wrapped, which will not be observable as a span *)
      let wraps =
        match turn with
        | Left v -> v / 100
        | Right v -> v / 100
      in
      (* Whether or not this turn passed zero as the remainder *)
      let passed =
        (* TODO I think this double counts exact 100 turns *)
        match turn with
        | Left _ -> (end_pos > start_pos && not (start_pos = 0)) || end_pos = 0
        | Right _ -> end_pos < start_pos || end_pos = 0
      in
      wraps + if passed then 1 else 0
  in
  let rec doit pos turns accum =
    match turns with
    | [] -> accum + count pos None
    | head :: tail ->
      let passes = count pos (Some head) in
      let next_pos = turn_dial pos head in
      (doit [@tailcall]) next_pos tail (passes + accum)
  in
  doit start_pos turns 0
;;

(** perform_turns returns a list of all positions the dial would stop at while performing the given turns. *)
let perform_turns turns start_pos =
  let rec doit pos turns accum =
    let accum = pos :: accum in
    match turns with
    | [] -> accum
    | head :: tail -> (doit [@tailcall]) (turn_dial pos head) tail accum
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
