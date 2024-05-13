open OUnit2

open Connections.Word
(**open Connections.Category**)

let tests =
  "tests"
  >::: [
         ( "make" >:: fun _ ->
           (*a basic test with a regular word and regular cattegory*)
           assert_equal test_word (make "hello" "greeting");
           (*test with a an empty string word and an empty string cattegory*)
           assert_equal test_word2 (make "" "");
           (*testing make with strings that have characters that are not
             letters*)
           assert_equal test_word3 (make "%^z+3" "/s@_{}[]") );
         ( "is_empty" >:: fun _ ->
           (*testing an empty t (both the word and the category is empty) *)
           assert_bool "failed1" (is_empty test_word2);
           (*testing a t with an empty word but non-empty category *)
           let a = make "" "category" in
           assert_bool "failed2" (is_empty a);
           (*testing a t with non-empty word and non-empty category *)
           assert_bool "failed3" (not (is_empty test_word));
           (*testing a t with a non-empty word and empty category*)
           let b = make "word" "" in
           assert_bool "failed4" (not (is_empty b)) );
       ]

let () = run_test_tt_main tests
