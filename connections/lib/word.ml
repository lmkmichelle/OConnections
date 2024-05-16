type t = {
  mutable word : string;
  mutable category : string;
}

let make word category = { word; category }
let is_empty word = word.word = ""
let test_word = { word = "hello"; category = "greeting" }
let test_word2 = { word = ""; category = "" }
let test_word3 = { word = "%^z+3"; category = "/s@_{}[]" }

let print_word wrd =
  Printf.printf "{word : \"%s\"; category : \"%s\"}" wrd.word wrd.category
