let () = Random.init (48708 + 47711)

(* std low-level *)
let stdout = Unix.descr_of_out_channel stdout
let inn = read_line () |> String.trim

(* file name from command line argument *)
let file =
  try Unix.openfile Sys.argv.(1) [ Unix.O_WRONLY; Unix.O_TRUNC ] 0
  with _ -> exit 1

let rec split = function
  | str when String.length str = 0 -> []
  | str when String.length str mod 2 = 0 ->
      String.sub str 0 2 :: split (String.sub str 2 (String.length str - 2))
  | str ->
      String.sub str 0 1 :: split (String.sub str 1 (String.length str - 1))

let rec invert_half i acc1 acc2 = function
  | [] -> List.rev_append acc1 (acc2 |> List.rev)
  | x :: xs when i < String.length inn / 4 ->
      invert_half (i + 1) (x :: acc1) acc2 xs
  | x :: xs -> invert_half (i + 1) acc1 (x :: acc2) xs

let code_to_string code = Char.chr code |> String.make 1

let rec random_chars = function
  | 0 -> []
  | n -> code_to_string (Random.int 26 + 97) :: random_chars (n - 1)

let random_str () =
  let rand = Random.int 15 in
  (random_chars rand |> List.fold_left (fun acc a -> acc ^ a) String.empty, rand)

let trashify x =
  let x_len = String.length x in
  let trash, trash_len = random_str () in
  let read = "r " ^ string_of_int x_len in
  (x_len, trash, trash_len, read)

let rec encode i last_len fst_half scnd_half commands = function
  | [] -> (fst_half ^ scnd_half, commands)
  | x :: xs when i < String.length inn / 4 ->
      let x_len, trash, trash_len, read = trashify x in
      let forward = "+ " ^ string_of_int trash_len in
      encode (i + 1) x_len
        (fst_half ^ x ^ trash)
        scnd_half
        (forward :: read :: commands)
        xs
  | x :: xs when i = String.length inn / 4 ->
      let x_len, trash, trash_len, read = trashify x in
      let endl = "f " ^ string_of_int (trash_len + x_len - 1) in
      (* EOF is the -1 *)
      encode (i + 1) x_len fst_half
        (scnd_half ^ x ^ trash)
        (read :: endl :: commands) xs
  | x :: xs ->
      let x_len, trash, trash_len, read = trashify x in
      let endl = "- " ^ string_of_int (trash_len + x_len + last_len) in
      encode (i + 1) x_len fst_half
        (x ^ trash ^ scnd_half)
        (read :: endl :: commands) xs

let result, commands =
  inn |> split |> invert_half 0 [] [] |> encode 0 1 "" "" [] |> fun (a, b) ->
  (String.to_bytes a, List.rev ("s 0" :: b))

let () =
  Unix.write file result 0 (Bytes.length result) |> ignore;
  List.iter
    (fun a ->
      let res = String.to_bytes (a ^ "\n") in
      Unix.write stdout res 0 (Bytes.length res) |> ignore)
    commands;
  Unix.close file;
  Unix.close stdout
