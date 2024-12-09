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

let all_lines = ref []

let load_input filepath =
  let count = ref 1 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    all_lines := line :: !all_lines;
    count := !count + 1)
;;

let _ = load_input "./input"

let valid_4 =
  [ [ 0, 0; 1, 0; 2, 0; 3, 0 ]
  ; [ 0, 0; 0, 1; 0, 2; 0, 3 ]
  ; [ 0, 0; 1, 1; 2, 2; 3, 3 ]
  ; [ 0, 3; 1, 2; 2, 1; 3, 0 ]
  ]
;;

let get_x (x, _y) = x
let get_y (_x, y) = y

let get4len_str_from_4indexes lst_lst indexes cur_pos =
  let char1 =
    try
      List.nth
        (List.of_seq
         @@ String.to_seq
         @@ List.nth lst_lst (get_x cur_pos + (get_x @@ List.nth indexes 0)))
      @@ (get_y cur_pos + (get_y @@ List.nth indexes 0))
    with
    | _ -> ' '
  in
  let char2 =
    try
      List.nth
        (List.of_seq
         @@ String.to_seq
         @@ List.nth lst_lst (get_x cur_pos + (get_x @@ List.nth indexes 1)))
      @@ (get_y cur_pos + (get_y @@ List.nth indexes 1))
    with
    | _ -> ' '
  in
  let char3 =
    try
      List.nth
        (List.of_seq
         @@ String.to_seq
         @@ List.nth lst_lst (get_x cur_pos + (get_x @@ List.nth indexes 2)))
      @@ (get_y cur_pos + (get_y @@ List.nth indexes 2))
    with
    | _ -> ' '
  in
  let char4 =
    try
      List.nth
        (List.of_seq
         @@ String.to_seq
         @@ List.nth lst_lst (get_x cur_pos + (get_x @@ List.nth indexes 3)))
      @@ (get_y cur_pos + (get_y @@ List.nth indexes 3))
    with
    | _ -> ' '
  in
  String.of_seq (List.to_seq [ char1; char2; char3; char4 ])
;;

let find_xmas str =
  match str with
  | "XMAS" -> 1
  | "SAMX" -> 1
  | _ -> 0
;;

(*  *)
let part1 () =
  let count = ref 0 in
  List.iteri
    (fun x l ->
      List.iteri
        (fun y _ ->
          let cur_pos = x, y in
          let lines_of_4 =
            List.map
              (fun indexes -> get4len_str_from_4indexes !all_lines indexes cur_pos)
              valid_4
          in
          let local = List.fold_left (fun acc t -> acc + find_xmas t) 0 lines_of_4 in
          count := !count + local)
        (List.of_seq @@ String.to_seq l))
    !all_lines;
  print_endline @@ "result: " ^ string_of_int !count
;;

let part2 () =
  let count = ref 0 in
  print_endline @@ "result: " ^ string_of_int !count
;;

(*  *)
let _ = part1 ()
let _ = part2 ()

(* Function to find all occurrences of a pattern in a text *)
