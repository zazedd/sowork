(* std low-level *)
let stdout, stderr =
  (Unix.descr_of_out_channel stdout, Unix.descr_of_out_channel stderr)

(* file name from command line argument *)
let file = try Unix.openfile Sys.argv.(1) [ Unix.O_RDONLY ] 0 with _ -> exit 1

(* main program *)
let rec file_manipulator buffer fsize =
  (* Reads a line from the input *)
  let inn = read_line () |> String.split_on_char ' ' in
  match inn with
  | [ "+"; n ] ->
      Unix.lseek file (int_of_string n) Unix.SEEK_CUR |> ignore;
      file_manipulator buffer fsize
  | [ "-"; n ] ->
      Unix.lseek file (int_of_string n * -1) Unix.SEEK_CUR |> ignore;
      file_manipulator buffer fsize
  | [ "i"; n ] ->
      Unix.lseek file 0 Unix.SEEK_SET |> ignore;
      Unix.lseek file (int_of_string n) Unix.SEEK_CUR |> ignore;
      file_manipulator buffer fsize
  | [ "f"; n ] ->
      (* Relative to the end | * -1 because the input will be positive and we want to walk backwards *)
      Unix.lseek file (int_of_string n * -1) Unix.SEEK_END |> ignore;
      file_manipulator buffer fsize
  | [ "r"; n ] ->
      let n = int_of_string n in
      let buffer = Bytes.sub buffer 0 fsize in
      let b = Bytes.create 101 in
      let nn = Unix.read file b 0 n in
      let b = Bytes.sub b 0 nn in
      let buffer = Bytes.concat Bytes.empty [ buffer; b ] in
      file_manipulator buffer (fsize + nn)
  | [ "l"; "0" ] -> file_manipulator (Bytes.map (fun _ -> Char.chr 0) buffer) 0
  | [ "s"; "0" ] ->
      (* Add '\n' *)
      let fbuffer = Bytes.concat Bytes.empty [ buffer; String.to_bytes "\n" ] in
      (* write the fbuffer to the output *)
      Unix.write stdout fbuffer 0 (Bytes.length fbuffer) |> ignore
  | _ ->
      let errmsg = String.to_bytes "Falta de argumentos\n" in
      Unix.write stderr errmsg 0 (Bytes.length errmsg) |> ignore;
      file_manipulator buffer fsize

let () = file_manipulator (Bytes.create 101) 0

let () =
  Unix.close stdout;
  Unix.close stderr;
  Unix.close file
