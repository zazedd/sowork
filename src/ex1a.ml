(* std low-level *)
let stdout, stderr =
  (Unix.descr_of_out_channel stdout, Unix.descr_of_out_channel stderr)

(* file name from command line argument *)
let file = try Unix.openfile Sys.argv.(1) [ Unix.O_RDONLY ] 0 with _ -> exit 1

(* main program *)
let rec file_manipulator buffer =
  (* Reads a line from the input *)
  let inn = read_line () |> String.split_on_char ' ' in
  match inn with
  | [ "+"; n ] ->
      Unix.lseek file (int_of_string n) Unix.SEEK_CUR |> ignore;
      file_manipulator buffer
  | [ "-"; n ] ->
      Unix.lseek file (int_of_string n * -1) Unix.SEEK_CUR |> ignore;
      file_manipulator buffer
  | [ "i"; n ] ->
      Unix.lseek file 0 Unix.SEEK_SET |> ignore;
      Unix.lseek file (int_of_string n) Unix.SEEK_CUR |> ignore;
      file_manipulator buffer
  | [ "f"; n ] ->
      (* Relative to the end | n + 1 because of EOF *)
      Unix.lseek file ((int_of_string n + 1) * -1) Unix.SEEK_END |> ignore;
      file_manipulator buffer
  | [ "r"; n ] ->
      let b = Bytes.create 101 in
      let _ = Unix.read file b 0 (int_of_string n) in
      let buffer = Bytes.concat Bytes.empty [ buffer; b ] in
      file_manipulator buffer
  | [ "l"; "0" ] -> file_manipulator (Bytes.map (fun _ -> Char.chr 0) buffer)
  | [ "s"; "0" ] ->
      let buffer = Bytes.concat Bytes.empty [ buffer; String.to_bytes "\n" ] in
      Unix.write stdout buffer 0 (Bytes.length buffer) |> ignore
  | _ ->
      let errmsg = String.to_bytes "Falta de argumentos\n" in
      Unix.write stderr errmsg 0 (Bytes.length errmsg) |> ignore;
      file_manipulator buffer

let () = file_manipulator (Bytes.create 101)

let () = Unix.close stdout; Unix.close stderr; Unix.close file
