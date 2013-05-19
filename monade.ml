type 'a wmonade = 'a*string
type action = Fork of (action*action) | Stop | Atom of (unit -> action wmonade)  
type 'a cmonade = ('a -> action) -> action
let creturn x = fun suite -> suite x
let cbind x f = fun a -> x (fun b -> f b a)   
let wreturn x = ((x,""))
let wbind x f = let (a,texte1) = x in let (b,texte2) = f a in (b, texte1^texte2)
let write s = print_string s; ((), s)
let lift monade = fun c -> Atom (fun () -> wbind monade (fun a -> wreturn (c a)))
let cwrite s = lift (write s)
let action c = c (fun x -> Stop)
let atom m = fun c -> Atom (fun () -> wbind m (fun a -> wreturn (c a))) 
let stop = fun x -> Stop
let par m1 m2 = fun c -> Fork (m1 c, m2 c)
let fork m = fun c -> Fork (action m, c () )
let rec round = function
   [] -> wreturn ()
  |a::q -> match a with
      Stop -> round q
     |Fork (a1, a2) -> round (q @ [a1; a2])
     |Atom a1 -> wbind (a1 ()) (function a -> round (q @ [a])) 
let run m = round [action m]
let afficher m = print_string (snd m)
let rec loop s = cbind (cwrite s) (function a -> loop s) 
let _ = let continuation = cbind (fork (loop "fish")) (function x -> loop "cat") in
 run continuation 
