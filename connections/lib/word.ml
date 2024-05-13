type t = {
  mutable word : string;
  mutable category : string;
}

let make s1 s2 = { word = s1; category = s2 }
let is_empty word = word.word = ""
let test_word = { word = "hello"; category = "greeting" }
let test_word2 = { word = ""; category = "" }
let test_word3 = { word = "%^z+3"; category = "/s@_{}[]" }
