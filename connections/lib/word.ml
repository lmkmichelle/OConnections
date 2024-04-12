type t = {
  word : string;
  category : string;
}

let make s1 s2 = { word = s1; category = s2 }
let test_word = { word = "hello"; category = "greeting" }
let test_word2 = { word = ""; category = "" }
