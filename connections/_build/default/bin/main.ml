
type data = 
{ 
  category : string;
  items : string list
}

let const = [
  {category = "Timekeeping devices"; items = ["Clock"; "Hourglass"; "Sundial"; "Watch"]};
  {category = "Units of measure"; items = ["Second"; "Newton"; "Hertz"; "Mole"]};
  {category = "Hairstyles"; items = ["Bob"; "Crop"; "Pixie"; "Shag"]};
  {category = "Dr. ___"; items = ["Evil"; "Pepper"; "J"; "No"]};
  ]

let game () = 
  for i = 0 to (List.length const) - 1 do
    let () = print_endline ((List.nth const i).category) in
    for j = 0 to List.length ((List.nth const i).items) - 1 do
      ANSITerminal.printf [ANSITerminal.black] "[%i: %s]" ((j + 1) + (i * 4)) (List.nth (List.nth const i).items j)
    done;
  done

let _ = game ()