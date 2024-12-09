module JaneStreet = struct
  include Core
  include Base
end

let consume_input_text_line_by_line ~filepath ~consume =
  let rec exhaust channel =
    match In_channel.input_line channel with
    | None -> ()
    | Some line ->
      consume line;
      exhaust channel
  in
  In_channel.with_open_text filepath (fun channel -> exhaust channel)
;;

let find_pattern regex text =
  let rec aux start acc =
    try
      let pos = Str.search_forward regex text start in
      let matched = Str.matched_string text in
      aux (pos + 1) (matched :: acc)
      (* Add the match to the accumulator *)
    with
    | Not_found -> List.rev acc (* Reverse the list before returning *)
  in
  aux 0 []
;;

let all_lines = ref []

let load_input filepath =
  let count = ref 1 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    all_lines := line :: !all_lines;
    count := !count + 1)
;;

let _ = load_input "./input"

let get_product s =
  let s_lst = Str.split (Str.regexp {|mul(\|,\|)|}) s in
  let s_lst2 = List.filter (fun s -> s != "") s_lst in
  let i_list = List.map (fun s -> int_of_string s) s_lst2 in
  let product = List.fold_left (fun acc i -> acc * i) 1 i_list in
  product
;;

(*  *)
let part1 () =
  let long_text = List.fold_left (fun acc l -> acc ^ l) "" (List.rev !all_lines) in
  print_endline @@ "long_text: " ^ long_text;
  print_endline @@ "long_text ends";
  let all_mul = find_pattern (Str.regexp {|mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)|}) long_text in
  List.iter (fun mul -> print_endline @@ "mul: " ^ mul) all_mul;
  let all_products = List.map (fun m -> get_product m) all_mul in
  let sum_of_all_products = List.fold_left (fun acc p -> acc + p) 0 all_products in
  print_endline @@ "result: " ^ string_of_int sum_of_all_products
;;

let part2 () = ()

(*  *)
let _ = part1 ()
let _ = part2 ()

(* Function to find all occurrences of a pattern in a text *)
