open Unix;;

let _ = 
let x = fork () in
  if x = 0 then print_string "Bonjour" else print_string "Adios"
