open OUnit2
open Aoc_2025

let printer_from (pp : Format.formatter -> 'a -> unit) (value : 'a) : string =
  pp Format.str_formatter value;
  Format.flush_str_formatter ()
;;

let string_of_positions =
  printer_from (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_int)
;;

let tests =
  "Dial Test Suite"
  >::: [ ("turn_dial"
          >:: fun _ ->
          assert_equal
            ~msg:"Right rollover from 99"
            ~printer:string_of_int
            0
            (Dial.turn_dial 99 @@ Right 1);
          assert_equal ~printer:string_of_int 10 (Dial.turn_dial 99 @@ Right 11);
          assert_equal ~printer:string_of_int 99 (Dial.turn_dial 0 @@ Left 1);
          assert_equal ~printer:string_of_int 89 (Dial.turn_dial 0 @@ Left 11);
          assert_equal ~printer:string_of_int 47 (Dial.turn_dial 50 @@ Left 703);
          assert_equal ~printer:string_of_int 37 (Dial.turn_dial 50 @@ Right 287))
       ; ("perform_turns"
          >:: fun _ ->
          assert_equal
            ~printer:string_of_positions
            [ 50; 51; 52; 53; 54; 55 ]
            (Dial.perform_turns [ Right 1; Right 1; Right 1; Right 1; Right 1 ] 50);
          assert_equal ~printer:string_of_positions [ 50 ] (Dial.perform_turns [] 50))
       ]
;;

let () = run_test_tt_main tests
