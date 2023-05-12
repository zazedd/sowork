open Unix

let () = Printexc.record_backtrace true
let perm = 0o640
let n = Sys.argv.(1) |> int_of_string

(* let p = Sys.argv.(2) *)
(* let t = Sys.argv.(3) *)
let max = Sys.argv.(4) |> int_of_string
let pid_father = getpid ()

let rec read_block f buf size =
  let n = read f buf 0 size in
  if n > 0 then n else read_block f buf size

let rec make_pipes = function
  | 1 ->
      let str = Format.sprintf "/tmp/pipe1to%d" n in
      mkfifo str perm;
      [ openfile str [ O_RDWR ] 0 ]
  | m ->
      let str = Format.sprintf "/tmp/pipe%dto%d" (m - 1) m in
      mkfifo str perm;
      openfile str [ O_RDWR ] 0 :: make_pipes (m - 1)

let rec wait_for_processes = function
  | 0 -> ()
  | n ->
      wait () |> ignore;
      wait_for_processes (n - 1)

let rec read_write_loop rd wr buf mm =
  let n = read_block rd buf (Bytes.length buf) in
  let buf = Bytes.sub buf 0 n in
  let s = buf |> Bytes.to_string in
  Format.printf "[mm:%d:%s]\n@." mm s;
  let token = (s |> int_of_string) + 1 in
  let token_bytes = token |> string_of_int |> Bytes.of_string in
  match token > max with
  | true -> ()
  | false ->
      write wr token_bytes 0 n |> ignore;
      read_write_loop rd wr (Bytes.create 1024) mm

let rec make_processes pipes = function
  | 0 -> ()
  | 1 -> (
      match fork () with
      | 0 ->
          (* Child process *)
          Format.printf "[m:%d]\n@." 1;
          let rd = List.nth pipes (n - 1) in
          let wr = List.nth pipes 0 in
          let buf = Bytes.create 1024 in
          read_write_loop rd wr buf 1
      | _ ->
          (* Parent process *)
          (* let first_pipe = List.nth pipes 0 in *)
          let buf = Bytes.of_string "0" in
          write stdout buf 0 (Bytes.length buf) |> ignore;
          make_processes pipes 0;
          wait_for_processes 0)
  | m -> (
      match fork () with
      | 0 ->
          (* Child process *)
          Format.printf "[m:%d]\n@." m;
          let rd = List.nth pipes (m - 2) in
          let wr = List.nth pipes (m - 1) in
          let buf = Bytes.create 1024 in
          read_write_loop rd wr buf m
      | _ ->
          (* Parent process *)
          (* let first_pipe = List.nth pipes 0 in *)
          (* let buf = Bytes.of_string "0" in *)
          (* write stdout buf 0 (Bytes.length buf) |> ignore; *)
          make_processes pipes (m - 1))

let pipes = make_pipes n
let () = make_processes pipes n

let () =
  if pid_father = getpid () then (
    let buf = Bytes.of_string "0" in
    write (List.hd pipes) buf 0 (Bytes.length buf) |> ignore;
    wait_for_processes n)
