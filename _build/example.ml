open Printexc

module Example (K : Kahn.S) = struct
  module K = K
  module Lib = Kahn.Lib(K)
  open Lib

  let integers (qo : int K.out_port) : unit K.process =
    let rec loop n =
      (K.put n qo) >>= (fun () -> loop (n + 1))
    in
    loop 2

  let output message (qi : int K.in_port) : unit K.process =
    let rec loop () =
      (K.get qi) >>= (fun v -> Format.printf "%s%d@." message v; loop ())
    in
    loop ()
  let pseudoalea p (qo : int K.out_port) : unit K.process =
    let rec loop () =
      (K.put (if p < Random.float 1. then 0 else 1) qo) >>= (fun v -> loop ())
    in loop ()
  let copy_and_branch (qi : int K.in_port) (qo1 : int K.out_port) (qo2 : int K.out_port) =
    let rec loop () = 
      (K.get qi) >>= (fun v -> ( (K.put v qo1) >>= (fun () -> K.put v qo2) >>= (fun () -> loop () ))) in loop ()
  let correction_newmann (qi : int K.in_port) (qo : int K.out_port) = 
    let rec loop () =
      (K.get qi) >>= (fun v1 -> (K.get qi ) >>= (fun v2 -> if v1 = 0 && v2 = 1 then K.put 0 qo else if v1 = 1 && v2 = 0 then K.put 1 qo else K.return ())>>= (fun () -> loop ()) ) 
    in loop ()
  let main : unit K.process =
    (delay K.new_channel ()) >>=
    (fun (q_in, q_out) -> K.doco [ (integers q_out) ; (output "Recu: " q_in) ; ])

  let main2 : unit K.process =
    let (qin1, qout1) = K.new_channel () and (qin2, qout2) = K.new_channel () and (qin3, qout3) = K.new_channel () and (qin4, qout4) = K.new_channel () in
      K.doco [pseudoalea 0.3 qout1; copy_and_branch qin1 qout2 qout3; output "pseudoalea: " qin2; correction_newmann qin3 qout4; output "rectifie: " qin4 ]

end

module E = Example(Kahn.Seq)

let () = record_backtrace true ; try E.K.run E.main2 with End_of_file -> print_backtrace stdout
