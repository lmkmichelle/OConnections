type data = 
{ 
  category : string;
  items : string list
}

type word = 
{
  word : string;
  category : string;
}

let const = [
  {category = "Timekeeping devices"; items = ["Clock"; "Hourglass"; "Sundial"; "Watch"]};
  {category = "Units of measure"; items = ["Second"; "Newton"; "Hertz"; "Mole"]};
  {category = "Hairstyles"; items = ["Bob"; "Crop"; "Pixie"; "Shag"]};
  {category = "Dr. ___"; items = ["Evil"; "Pepper"; "J"; "No"]};
]

let rec convert_to_int_list acc = function
| [] -> List.rev acc
| hd :: tl ->
    let num = int_of_string hd in
    convert_to_int_list (num :: acc) tl

let game () = 
  let list_of_words = ref [] in
  for i = 0 to (List.length const) - 1 do
    for j = 0 to List.length ((List.nth const i).items) - 1 do
      list_of_words := !list_of_words @ [{word = (List.nth (List.nth const i).items j); category = (List.nth const i).category}]
    done
  done;

  for words = 0 to (List.length !list_of_words - 1) do 
    if (words mod 4 = 0 && words <> 0) then 
      let () = print_newline () in
      ANSITerminal.printf [ANSITerminal.black] "[%i: %s]" (words + 1) (List.nth !list_of_words words).word
    else 
      ANSITerminal.printf [ANSITerminal.black] "[%i: %s]" (words + 1) (List.nth !list_of_words words).word
  done;

  let () = print_endline "Please enter the numbers of the four words you would like to guess, with a space in between." in
  let the_input = read_line () in
  let numbers = 
    let split_input = String.split_on_char ' ' the_input in
    convert_to_int_list [] split_input in
if List.length numbers = 4 then 
  let word1_category = (List.nth !list_of_words ((List.nth numbers 0) - 1)).category in
  let word2_category = (List.nth !list_of_words ((List.nth numbers 1) - 1)).category in
  let word3_category = (List.nth !list_of_words ((List.nth numbers 2) - 1)).category in
  let word4_category = (List.nth !list_of_words ((List.nth numbers 3) - 1)).category in
  
  if word1_category = word2_category && 
     word1_category = word3_category && 
     word1_category = word4_category then
     print_endline "Correct!" 
  else
     (* Categories don't match *)
     (* Your logic here *)
    print_endline "Nope!" 
else
  print_endline ("Not a four letter word.")



  
  
let _ = game ()
