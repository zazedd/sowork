open Unix

let perm = 0o640
let n = Sys.argv.(1) |> int_of_string
let p = Sys.argv.(2) |> float_of_string
let t = Sys.argv.(3) |> float_of_string
let timeout = Sys.argv.(4) |> float_of_string
let time_start = Unix.time ()
let pid_father = getpid ()
let exit_file = openfile "exit.txt" [ O_CREAT; O_RDWR ] 0
let buf = Bytes.of_string "0"
let _ = write exit_file buf 0 (Bytes.length buf)

let rec make_pipes = function
  | 1 ->
      let str = Format.sprintf "/tmp/pipe%dto1" n in
      mkfifo str perm;
      [ openfile str [ O_RDWR ] 0 ]
  | m ->
      let str = Format.sprintf "/tmp/pipe%dto%d" (m - 1) m in
      mkfifo str perm;
      openfile str [ O_RDWR ] 0 :: make_pipes (m - 1)

let read_block rd buf len =
  let n = read rd buf 0 len in
  match n with 0 -> None | b -> Some b

let rec read_write_loop rd wr buf mm =
  let should_exit = Bytes.create 1 in
  let _ = lseek exit_file 0 SEEK_SET in
  let _ = read exit_file should_exit 0 1 in
  match should_exit |> Bytes.to_string with
  | "1" ->
      Format.printf "[p%d] Leaving\n@." mm;
      exit 1
  | "0" | _ -> (
      let n = read_block rd buf (Bytes.length buf) in
      match n with
      | None -> read_write_loop rd wr buf mm
      | Some b -> (
          let buf = Bytes.sub buf 0 b in
          let s = buf |> Bytes.to_string in
          match s with
          | "e" ->
              Format.printf "[p%d] Leaving\n@." mm;
              let s = "e" |> Bytes.of_string in
              write wr s 0 (Bytes.length s) |> ignore;
              exit 0 (* Exit because exit message recieved*)
          | _ -> (
              let () = Random.self_init () in
              let rand_num = Random.float 1.0 in
              let token = (s |> int_of_string) + 1 in
              let () =
                if rand_num < p then
                  let () =
                    Format.printf "[p%d] blocked on token (val = %d)\n@." mm
                      token
                  in
                  sleepf t
                else ()
              in
              Format.printf "[Process:%d;Value:%s]\n@." mm s;
              let token = (s |> int_of_string) + 1 in
              let token_bytes = token |> string_of_int |> Bytes.of_string in
              match Unix.time () -. time_start > timeout with
              | true ->
                  Format.printf
                    "Timeout exceeded, sending exit message, leaving@.";
                  let s = "e" |> Bytes.of_string in
                  write wr s 0 (Bytes.length s) |> ignore;
                  exit 0 (* Send exit message and exit process *)
              | false ->
                  write wr token_bytes 0 (Bytes.length token_bytes) |> ignore;
                  read_write_loop rd wr (Bytes.create 1024) mm)))

let rec make_processes pipes n = function
  | i when i = n -> ()
  | 0 -> (
      let pid = fork () in
      match pid with
      | 0 ->
          (* Child process *)
          let rd = List.nth pipes (n - 1) in
          let wr = List.nth pipes 0 in
          let buf = Bytes.create 1024 in
          read_write_loop rd wr buf 1
      | _ ->
          (* Parent process *)
          make_processes pipes n 1)
  | i -> (
      let pid = fork () in
      match pid with
      | 0 ->
          (* Child process *)
          let rd = List.nth pipes (i - 1) in
          let wr = List.nth pipes i in
          let buf = Bytes.create 1024 in
          read_write_loop rd wr buf (i + 1)
      | _ ->
          (* Parent process *)
          make_processes pipes n (i + 1))

let () =
  if pid_father = getpid () then (
    let pipes = make_pipes n in
    let () = make_processes pipes n 0 in
    write (List.hd pipes) buf 0 (Bytes.length buf) |> ignore;
    let _ = lseek exit_file 0 SEEK_SET in
    wait () |> ignore;
    wait () |> ignore;
    close exit_file)
