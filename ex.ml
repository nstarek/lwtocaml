#require "lwt.unix";;
open Base;;
let (>>=) = Lwt.bind
let return = Lwt.return

let alarme (f:float) =
  Lwt_unix.sleep f
  >>= fun () -> Lwt_io.fprintf Lwt_io.stdout "debout %f" f;;
Lwt.join [alarme 5.0; alarme 4.0; alarme 3.0];;

let alarme_en_paralle (l:float list) =
  Lwt.join (List.map l alarme)

let alarme_utilisateur () =
  Lwt_io.read_line Lwt_io.stdin
  >>= fun st -> alarme (Float.of_string st)

let rec rec_alarm () =
  Lwt_io.read_line Lwt_io.stdin
  >>= fun st -> Lwt.join [alarme (Float.of_string st); rec_alarm ()]
