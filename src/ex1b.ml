
let inn = read_line () |> String.trim

let rec split = function
  | str when String.length str = 0 -> []
  | str when String.length str mod 2 = 0 -> String.sub str 0 2 :: split (String.sub str 2 (String.length str - 2))
  | str -> String.sub str 0 1 :: split (String.sub str 1 (String.length str - 1))

let code_to_string code = Char.chr code |> String.make 1

let rec random_chars = function
  | 0 -> []
  | n -> code_to_string (Random.int 26 + 97) :: random_chars (n - 1)

let random_str () = let rand = Random.int 15 in random_chars rand |> List.fold_left (fun acc a -> acc ^ a) String.empty, rand

let rec encode result commands = function
  | [] -> result, commands
  | x :: xs -> begin 
    let trash, tr_len = random_str () in
    let forward = "+ " ^ string_of_int tr_len in
    let read = "r " ^ string_of_int (String.length x) in
    encode (result ^ x ^ trash) (forward :: read :: commands) xs
  end

let () = List.iter (fun a -> Printf.printf "%s\n" a) (inn |> split)

let result, commands = inn |> split |> encode "" [] |> (fun (a, b) -> a, List.rev b)

let () = print_endline result; 
         List.iter (fun a -> print_endline a) commands

