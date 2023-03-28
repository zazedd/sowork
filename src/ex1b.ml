let () = Random.init (48708 + 47711)
(* std low-level *)
(* let stdout, stderr = *)
(*   (Unix.descr_of_out_channel stdout, Unix.descr_of_out_channel stderr) *)

let inn = read_line () |> String.trim

let rec split = function
  | str when String.length str = 0 -> []
  | str when String.length str mod 2 = 0 -> String.sub str 0 2 :: split (String.sub str 2 (String.length str - 2))
  | str -> String.sub str 0 1 :: split (String.sub str 1 (String.length str - 1))

let rec invert_half i acc1 acc2 = function
  | [] -> List.rev_append acc1 (acc2 |> List.rev)
  | x :: xs when i < ((String.length inn) / 4) -> invert_half (i + 1) (x :: acc1) acc2 xs
  | x :: xs -> invert_half (i + 1) acc1 (x :: acc2) xs

let code_to_string code = Char.chr code |> String.make 1

let rec random_chars = function
  | 0 -> []
  | n -> code_to_string (Random.int 26 + 97) :: random_chars (n - 1)

let random_str () = let rand = Random.int 15 in random_chars rand |> List.fold_left (fun acc a -> acc ^ a) String.empty, rand

let rec encode i last fst_half scnd_half commands = function
  | [] -> fst_half ^ scnd_half, commands
  | x :: xs when i < ((String.length inn) / 4) -> begin 
    let xlen = String.length x in
    let trash, tr_len = random_str () in
    let forward = "+ " ^ string_of_int tr_len in
    let read = "r " ^ string_of_int xlen in
    encode (i + 1) xlen (fst_half ^ x ^ trash) scnd_half (forward :: read :: commands) xs
  end
  | x :: xs when i = ((String.length inn) / 4) -> begin 
    let xlen = String.length x in
    let trash, tr_len = random_str () in
    let endl = "f " ^ string_of_int (tr_len + xlen) in
    let read = "r " ^ string_of_int xlen in
    encode (i + 1) xlen fst_half (scnd_half ^ x ^ trash) (read :: endl :: commands) xs
  end
  | x :: xs -> begin 
    let xlen = String.length x in
    let trash, tr_len = random_str () in
    let endl = "- " ^ string_of_int (tr_len + xlen + last) in
    let read = "r " ^ string_of_int xlen in
    Printf.printf "%s\n" (endl ^ read);
    encode (i + 1) xlen fst_half (x ^ trash ^ scnd_half) (read :: endl :: commands) xs
  end

let result, commands = inn |> split |> invert_half 0 [] [] |> encode 0 0 "" "" [] |> (fun (a, b) -> a, List.rev ("s 0" :: b))

let () = print_endline result; 
         List.iter (fun a -> print_endline a) commands 

