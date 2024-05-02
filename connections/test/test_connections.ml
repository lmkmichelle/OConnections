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
           assert_equal test_word2 (make "" "") );
         (* ( "make_catgeory_list" >:: fun _ ->
           let test_file = "test_data.txt" in
           assert_equal (make_category_list test_file) test_category ); *)
       ]

let () = run_test_tt_main tests
