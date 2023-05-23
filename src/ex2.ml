open Unix

let () = Printexc.record_backtrace true
let perm = 0o640
let n = Sys.argv.(1) |> int_of_string
let p = Sys.argv.(2) |> float_of_string
let t = Sys.argv.(3) |> float_of_string
let max = Sys.argv.(4) |> int_of_string
let pid_father = getpid ()

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
  let n = read_block rd buf (Bytes.length buf) in
  match n with
  | None -> read_write_loop rd wr buf mm
  | Some b -> (
      let buf = Bytes.sub buf 0 b in
      let s = buf |> Bytes.to_string in
      match s with
      | "e" ->
          Format.printf "Got exit message, sending exit message, leaving@.";
          let s = "e" |> Bytes.of_string in
          write wr s 0 (Bytes.length s) |> ignore;
          close rd;
          exit 0 (* Exit because exit message recieved*)
      | _ -> handle_read_block rd wr mm s)

and handle_read_block rd wr mm s =
  let () = Random.self_init () in
  let rand_num = Random.float 1.0 in
  let () =
    if rand_num < p then
      let () = Format.printf "Blocking process %d\n@." mm in
      sleepf t
    else ()
  in
  Format.printf "[Process:%d;Value:%s]\n@." mm s;
  let token = (s |> int_of_string) + 1 in
  let token_bytes = token |> string_of_int |> Bytes.of_string in
  match token > max with
  | true ->
      Format.printf "End reached, sending exit message, leaving@.";
      let s = "e" |> Bytes.of_string in
      write wr s 0 (Bytes.length s) |> ignore;
      close rd;
      exit 0 (* Send exit message and exit process *)
  | false ->
      write wr token_bytes 0 (Bytes.length token_bytes) |> ignore;
      read_write_loop rd wr (Bytes.create 1024) mm

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

let rec cleanup = function
  | 1 ->
      let str = Format.sprintf "/tmp/pipe%dto1" n in
      Unix.unlink str
  | m ->
      let str = Format.sprintf "/tmp/pipe%dto%d" (m - 1) m in
      Unix.unlink str;
      cleanup (m - 1)

let () =
  if pid_father = getpid () then (
    let pipes = make_pipes n in
    let () = make_processes pipes n 0 in
    let buf = Bytes.of_string "0" in
    write (List.hd pipes) buf 0 (Bytes.length buf) |> ignore;
    wait () |> ignore;
    cleanup n)
