open Unix

let () = Printexc.record_backtrace true
let perm = 0o640
let n = Sys.argv.(1) |> int_of_string

(* let p = Sys.argv.(2) *)
(* let t = Sys.argv.(3) *)
let max = Sys.argv.(4) |> int_of_string

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

let rec make_processes pipes = function
  | 0 -> ()
  | m -> (
      match fork () with
      | 0 -> (
          (* Child process *)
          let rd = List.nth pipes (m - 2) in
          let wr = List.nth pipes (m - 1) in
          let buf = Bytes.create 1024 in
          let n = read rd buf 0 1024 in
          let token = (buf |> Bytes.to_string |> int_of_string) + 1 in
          let token_bytes = token |> string_of_int |> Bytes.of_string in
          match token > max with
          | true -> ()
          | false -> write wr token_bytes 0 n |> ignore)
      | _ ->
          (* Parent process *)
          (* let first_pipe = List.nth pipes 0 in *)
          write_substring stdout "0" 0 1024 |> ignore;
          make_processes pipes (m - 1);
          wait_for_processes m)

let pipes = make_pipes n
let () = make_processes pipes n
