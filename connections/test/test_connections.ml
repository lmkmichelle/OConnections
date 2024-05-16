open OUnit2
open Connections.Word
open Connections.Category

let const_oab_test attempted =
  let ints = Array.make 4 0 in
  assert_equal
    (Some (Connections.Game.OutsideArchiveBounds { attempted }))
    (try
       let c =
         Connections.Game.const ("Archive~" ^ string_of_int attempted) ints
       in
       None
     with e -> Some e)

let tests =
  "tests"
  >::: [
         ( "word_make" >:: fun _ ->
           (*a basic test with a regular word and regular category*)
           assert_equal test_word (Connections.Word.make "hello" "greeting");
           (*test with a an empty string word and an empty string category*)
           assert_equal test_word2 (Connections.Word.make "" "");
           (*testing make with strings that have characters that are not
             letters*)
           assert_equal test_word3 (Connections.Word.make "%^z+3" "/s@_{}[]") );
         ( "word_is_empty" >:: fun _ ->
           (*testing an empty t (both the word and the category is empty) *)
           assert_bool "failed1" (is_empty test_word2);
           (*testing a t with an empty word but non-empty category *)
           let a = Connections.Word.make "" "category" in
           assert_bool "failed2" (is_empty a);
           (*testing a t with non-empty word and non-empty category *)
           assert_bool "failed3" (not (is_empty test_word));
           (*testing a t with a non-empty word and empty category*)
           let b = Connections.Word.make "word" "" in
           assert_bool "failed4" (not (is_empty b)) );
         ( "category_make" >:: fun _ ->
           (*creating a category with empty items*)
           assert_equal test_category (make "string" "string" [||] "string");
           (*creating a category with only one word in items*)
           assert_equal test_category2
             (make "string" "string" [| test_word |] "string");
           (*creating a category with multiple words in items*)
           assert_equal test_category3
             (make "string" "string"
                [| test_word; test_word2; test_word3 |]
                "string");
           (*using different strings for name, hint and difficulty fields*)
           assert_equal test_category4
             (make "abcd" "hi" [| test_word; test_word2; test_word3 |] "4") );
         ( "category_contains" >:: fun _ ->
           (*testing that the contains function does not detect any words within
             a category that has empty items field*)
           assert_bool "failed5" (not (contains test_word test_category));
           (*testing that the contains function does not detect an empty word
             within a category that has empty items field*)
           assert_bool "failed6" (not (contains test_word2 test_category));
           (*testing that contains function detects a word when it's in a
             category's items field *)
           assert_bool "failed7" (contains test_word test_category2);
           (*testing that contains function does not detect another word that is
             not existent in the items field *)
           assert_bool "failed8" (not (contains test_word2 test_category2));
           (*testing that contains function can still detect a word in items
             field of a category when the items field has multiple words*)
           assert_bool "failed9" (contains test_word test_category3);
           (*testing that contains function can detect all the word in items
             field of a category when the items field has multiple words*)
           assert_bool "failed10" (contains test_word2 test_category3);
           (*testing that contains function can detect all the word in items
             field of a category when the items field has multiple words*)
           assert_bool "failed10" (contains test_word3 test_category3) );
         ( "category_diff" >:: fun _ ->
           (*diff function finds the difficulty of the catgeory correctly**)
           assert_equal (diff test_category) "string";
           (*diff function finds the difficulty of the category correctly**)
           assert_equal (diff test_category4) "4" );
         ( "category_name" >:: fun _ ->
           (*name function finds the name of the catgeory correctly**)
           assert_equal (name test_category) "string";
           (*name function finds the name of the category correctly**)
           assert_equal (name test_category4) "abcd" );
         ( "category_items" >:: fun _ ->
           (*items function finds the items of the category correctly**)
           assert_equal (items test_category) [||];
           (*items function finds the items of the category correctly when there
             are multiple items**)
           assert_equal (items test_category4)
             [| test_word; test_word2; test_word3 |] );
        ("exception OutsideArchiveBounds with const"  >:: fun _ ->
          (**const run on archive with input > 39 raises OutsideArchiveBounds 
              correctly*)
          const_oab_test 40;
          (**const run on archive with input not two digits long raises OutsideArchiveBounds correctly*)
          const_oab_test 1;
          (**const run on archive with negative input raises OutsideArchiveBounds correctly*)
          const_oab_test ~-10;

        )
       ]

let () = run_test_tt_main tests
