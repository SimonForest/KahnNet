open Unix
open Marshal
open Mutex
  (*
module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end
*)
module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end
module Lib (K : S) = struct
  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo =  K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end


module Th: S = struct
  type 'a process = (unit -> 'a)

  type 'a channel = { q: 'a Queue.t ; m: Mutex.t; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () =
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

  let put v c () =
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

  let rec get c () =
    try
      Mutex.lock c.m;
      let v = Queue.pop c.q in
      Mutex.unlock c.m;
      v
    with Queue.Empty ->
      Mutex.unlock c.m;
      Thread.yield ();
      get c ()

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

  let run e = e ()
end

module Pr:S = struct
  type 'a process = (unit -> 'a)
  type 'a in_port = in_channel 
  type 'a out_port = out_channel

  let new_channel = fun () ->
    let (fdread, fdwrite) = pipe () in
      (in_channel_of_descr fdread, out_channel_of_descr fdwrite)
  let put valeur port () = Marshal.to_channel port valeur [Marshal.Closures] 
  let get (port : 'a in_port) () = (Marshal.from_channel port : 'a) 
  exception Child of (unit -> unit)
                              
  let doco l () = 
   try let prs = List.map (fun f -> let res = fork () in if res = 0 then raise (Child f) else res) l in
  List.iter (fun pidchild -> waitpid [] pidchild; ()) prs 
   with Child f -> (Format.printf "Enfant lance\n"; f () ; exit 0) 

  let return x = fun () -> x 
  let bind p f () = f (p ()) ()

  let run p = p ()
end

let create_dialog readport writeport emptyqueue =
  print_string "Bling Bling";
  print_newline ();
  let mutex = Mutex.create () and queue = emptyqueue in
  let receveur () = 
    let sock_addr = ADDR_INET (inet_addr_of_string "127.0.0.1", writeport) in
    let sock_serveur = socket (domain_of_sockaddr sock_addr) SOCK_STREAM 0 in
     bind sock_serveur sock_addr; listen sock_serveur 5;
     let rec loop () = sleep 2 ; Thread.yield ();let sockclient = fst (accept sock_serveur) in
     let chan = in_channel_of_descr sockclient and chan2 = out_channel_of_descr sockclient in
       print_string "Acceptation de l'envoi"; print_newline ();
       to_channel chan2 3 [Closures]; flush chan2;
     let (x:int) = (let rec f () = try select [sockclient] [] [] 0.2; from_channel chan with End_of_file -> f () in f ()) in
     lock mutex;   print_int x;print_newline (); Queue.add x queue; unlock mutex; close_in chan;
     loop ()
     in loop ()
  in Thread.create receveur (); 
    let sock_addr = ADDR_INET (inet_addr_of_string "127.0.0.1", readport) in
    let sock_serveur = socket (domain_of_sockaddr sock_addr) SOCK_STREAM 0 in
     bind sock_serveur sock_addr; listen sock_serveur 5;
     print_string "Serveur de renvoi lance";
     let rec loop () = Thread.yield (); if Queue.is_empty queue then (loop ()) else let sockclient = (let q = fst (accept sock_serveur) in q ) in
     let chan = out_channel_of_descr sockclient in
     lock mutex; let x = Queue.pop queue in 
       to_channel chan x [Closures]; unlock mutex; close_out chan; 
       loop ()
     in loop ()

let get_socket_to_port port =
  let sock_addr = ADDR_INET (inet_addr_of_string "127.0.0.1", port) in
  let sock = socket (domain_of_sockaddr sock_addr) SOCK_STREAM 0 in
  let rec loop () = try connect sock sock_addr with Unix_error (e,_,_) when e = ECONNREFUSED -> loop () in
    loop (); sock

module  PrSocket:S = struct
  type 'a process = (unit -> 'a)
  type 'a in_port = int 
  type 'a out_port = int
  let port_actu = ref 11115
  let get_new_port () = incr port_actu; !port_actu
  let new_channel () = 
    let readport = get_new_port () and writeport = get_new_port () in
  let x = fork () in if x = 0 then (create_dialog readport writeport (Queue.create () : 'a Queue.t); (0,0)) 
  else ((readport,writeport) :'a in_port * 'a out_port)


  let put valeur port () = let fd = get_socket_to_port port in
  let chan = out_channel_of_descr fd and chan2 = in_channel_of_descr fd in
    print_string "Truc foireux"; print_newline ();
    (from_channel chan2 : int); print_string "Premiere Reception"; print_newline (); to_channel chan valeur [Closures]; print_string "Put";  print_newline (); close_out chan
  let get (port : 'a in_port) () = let fd = get_socket_to_port port in
  let chan = in_channel_of_descr fd in
  print_string "tentative"; print_newline (); let x = (Marshal.from_channel chan : 'a) in print_string "Reussite"; print_newline (); close_in chan; x 
  
  
  exception Child of (unit -> unit)
                              
  let doco l () = 
   try let prs = List.map (fun f -> let res = fork () in if res = 0 then raise (Child f) else res) l in
  List.iter (fun pidchild -> waitpid [] pidchild; ()) prs 
   with Child f -> (f () ; exit 0) 

  let return x = fun () -> x 
  let bind p f () = f (p ()) ()

  let run p = p ()
end

module Seq:S = struct

  type action = Atom of (unit -> action) | Stop | Fork of (action * action)
  type 'a process = ('a -> action) ->  action
  type 'a in_port = 'a Queue.t
  type 'a out_port = 'a Queue.t

  let new_channel () = let q = Queue.create () in
    (q,q)
  let put valeur port = fun f -> Atom (fun () -> f (Queue.add valeur port))   
  let rec get port = fun f -> Atom (fun () -> if Queue.is_empty port then get port f else f (Queue.pop port))

  let doco liste = let rec aux = function
      [] -> (fun f -> Stop)
    | (a::q) -> (fun f -> Fork ( a f, (aux q) f) )
  in 
  let acheve = ref (List.length liste) in
    function suite -> let f () = decr acheve; if !acheve = 0 then suite () else Stop in aux liste f


  let return a = (fun f -> f a)
  let bind x f = (fun g -> x (fun r -> f r g))
  exception Retour
  let rec round = function
      [] -> failwith "Oups"
    | a :: q -> match a with 
          Atom f -> let x = f () in round (q @ [x])
        | Fork (a1, a2) -> round (q @ [a1; a2])
        | Stop -> round q
  let run p = let res = ref None in try (round [ p (fun x -> res := (Some x); raise Retour) ]) with Retour -> (match !res with Some x -> x)
end
