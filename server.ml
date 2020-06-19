open Base
(* open Lwt *)

type player = {input: Lwt_io.input_channel; output: Lwt_io.output_channel}

let (>>=) = Lwt.(>>=)
let return = Lwt.return
let (let*) = Lwt.bind


let sendMsg (m:string) (lp:player list) :  player list Lwt.t =
      let _ = (Lwt_list.map_p (fun p -> Lwt_io.fprintf p.output "%s" m >>= fun () -> Lwt_io.flush p.output) lp) in return lp


let rec get_answer (lp:player list) : (player*string) list Lwt.t = 
        match lp with 
        |[]-> return []
        |h::q -> Lwt_io.read_line h.input
         >>= fun st -> (get_answer q) >>= (fun l -> return ((h,st) :: l)) 


let filter_winners (r:string) (lr: (player * string) list) : player list Lwt.t = 
    Lwt_list.filter_p(fun (_,rep) -> return (String.equal r rep)) lr>>= Lwt_list.map_p(fun (x,_)-> return x) 


let filter_Fastest (lp: player list) : player list Lwt.t = 
  Lwt.choose (List.map ~f:(fun p->Lwt_io.read_line p.input>>= fun _ -> return [p]) lp)

 

let filter_faster_correct (r:string) (lp: player list): player list Lwt.t =
  let rec find_correct (l:(player*string) Lwt.t list) : player list Lwt.t=
      match List.length l with 
      |0 -> return []
      |_ -> Lwt.nchoose_split l 
            >>= fun(ready,pending) -> 
                 Lwt_list.filter_p (fun (_,rep) -> return (String.equal r rep)) ready
            >>= Lwt_list.map_p (fun (x,_)-> return x) 
            >>= fun res -> ( match res with 
                                    |[] -> find_correct pending
                                    |a::_ -> return [a]
                            )
 in
    find_correct (List.map ~f:(fun p->Lwt_io.read_line p.input >>= fun st -> return (p,st)) lp)


let getPlayers n : player list Lwt.t =
  let sockaddr = Lwt_unix.ADDR_INET (UnixLabels.inet_addr_loopback, 3003) in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec sock ;
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true ;
  Lwt_unix.bind sock sockaddr >>= fun () ->
  Lwt_unix.listen sock 3003 ;
  let rec acceptPlayer n acc : player list Lwt.t =
    if n > 0 then
      let pt =
        Lwt_unix.accept sock >>= fun (cliFD, _sock) ->
        let inputChan = Lwt_io.of_fd ~mode:Lwt_io.input cliFD in
        let outputChan = Lwt_io.of_fd ~mode:Lwt_io.output cliFD in
        {input=inputChan; output= outputChan} |> return
      in
      pt >>= fun p ->
      acceptPlayer (n - 1) (p :: acc)
    else
      acc |> return
  in
  acceptPlayer n []



let closePlayers listPlayers =
  Lwt_list.map_p
    (fun  player -> Lwt_io.close player.input)
    listPlayers

let _ =
  Lwt_main.run

    (* création des player *)
    (
      (Lwt_io.fprintf Lwt_io.stderr "Attente des joueurs...\n") >>=
      fun () -> let threadListPlayers = getPlayers 2 in
     (* actions *)
      threadListPlayers >>=
      fun listPlayers -> (* fonction stupide à changer *) return listPlayers >>=
       sendMsg "Ocaml est assez cool : vrai ou faux ?\n" >>=
      get_answer >>=
      filter_winners "vrai" >>=sendMsg  "Bravo!\n" >>=  
      
      sendMsg "Javascript est mieux qu'Ocaml: vrai ou faux ?\n" >>=
      get_answer >>=
      filter_winners "faux" >>=sendMsg  "Bravo!\n">>=
      
      sendMsg "Question de rapidité, avec quoi programment les vrais programmeurs : 1) nano, 2) emacs, 3) vi, 4) des papillons ?" >>=
     (* filter_Fastest  >>= 'ou' *)filter_faster_correct "4" >>=
      sendMsg  "Bravo!"  


     (* fermeture des player *)
     (* on reprend "threadListPlayers" pour être sur de tous les fermer *)
     >>= fun _ -> threadListPlayers >>= closePlayers)


