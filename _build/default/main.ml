module JaneStreet = struct
  include Core
  include Base
end
(* let consume_input_text_line_by_line ~filepath ~consume =
  let rec exhaust channel =
    match In_channel.input_line channel with
    | None -> ()
    | Some line ->
      consume line;
      exhaust channel
  in
  In_channel.with_open_text filepath (fun channel -> exhaust channel)
;;

(* let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst *)

(* let left_and_right_consume_input_text_line_by_line
   ~filepath
   ~condition
   ~left
   ~right
   ?first
   ?last
   ()
   =
   let continue = ref true in
   let rec loop ~condition ~left ~right ~first ~last ~channel =
   (match first with
   | None -> ()
   | Some f -> f ());
   (match condition () with
   | true -> continue := left channel
   | false -> right ());
   (match last with
   | None -> ()
   | Some f -> f ());
   if continue.contents then loop ~condition ~left ~right ~first ~last ~channel
   in
   ; ()In_channel.with_open_text filepath (fun channel ->
   loop ~condition ~left ~right ~first ~last ~channel)
   ;; *)

(* let unique lst =
   let rec aux l acc =
   match l with
   | [] -> acc
   | h :: t -> if List.mem h acc then aux t acc else aux t (h :: acc)
   in
   aux lst []
   ;; *)
(* todo *)

let if_numeric c = 
  try (int_of_char c) |> ignore; (int_of_char c) >= 48 && (int_of_char c < 58)
  with Failure _ -> false

let extract_numerics_from_line line =
   List.rev @@ String.fold_left (fun acc c -> if if_numeric c then ((int_of_char c) - 48) ::acc else acc) [] line
;;

let head_and_tail lst = 
int_of_string ((string_of_int @@ (List.hd lst)) ^ 
  (string_of_int ( List.hd @@ List.rev lst) )) 

let load_input filepath =
  let sum = ref 0 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line -> 
    let lst_of_ints = extract_numerics_from_line line in
    print_string @@ ("hd" ^ string_of_int @@ List.hd lst_of_ints) ^ "::";
    print_string @@ ("tail" ^ string_of_int @@ List.hd @@ List.rev lst_of_ints) ^ "\n";
    let i = head_and_tail lst_of_ints in
    sum := !sum + i
  ); 
  print_endline @@ string_of_int !sum
   

(*  *)
let run () = 
  load_input "./input"
;;
(*  *)
let _ = run () *)

(*  *)

(* p2 *)

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

(* let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst *)

(* let left_and_right_consume_input_text_line_by_line
   ~filepath
   ~condition
   ~left
   ~right
   ?first
   ?last
   ()
   =
   let continue = ref true in
   let rec loop ~condition ~left ~right ~first ~last ~channel =
   (match first with
   | None -> ()
   | Some f -> f ());
   (match condition () with
   | true -> continue := left channel
   | false -> right ());
   (match last with
   | None -> ()
   | Some f -> f ());
   if continue.contents then loop ~condition ~left ~right ~first ~last ~channel
   in
   ; ()In_channel.with_open_text filepath (fun channel ->
   loop ~condition ~left ~right ~first ~last ~channel)
   ;; *)

(* let unique lst =
   let rec aux l acc =
   match l with
   | [] -> acc
   | h :: t -> if List.mem h acc then aux t acc else aux t (h :: acc)
   in
   aux lst []
   ;; *)
(* todo *)

let extract_numerics_from_line line i =
  print_endline @@ "line#" ^ string_of_int i ^ ":" ^ line;
  let strs = Str.split (Str.regexp "   ") line in
  List.map int_of_string strs
;;

let all_pairs = ref []

let load_input filepath =
  let count = ref 1 in
  consume_input_text_line_by_line ~filepath ~consume:(fun line ->
    all_pairs := extract_numerics_from_line line !count :: !all_pairs;
    count := !count + 1)
;;

let _ = load_input "./input"

(*  *)
let part1 () =
  let left = ref [] in
  let right = ref [] in
  List.iter
    (fun ls ->
      let hd = List.hd ls in
      let last = List.nth ls 1 in
      left := hd :: !left;
      right := last :: !right)
    !all_pairs;
  left := List.sort compare !left;
  right := List.sort compare !right;
  let total_dis = ref 0 in
  List.iter
    (fun _ ->
      let l = List.hd !left in
      let r = List.hd !right in
      total_dis := !total_dis + abs (l - r);
      left := List.tl !left;
      right := List.tl !right)
    (List.init (List.length !left) (fun i -> i));
  print_endline @@ "result: " ^ string_of_int !total_dis
;;

let unique lst =
  let sorted = List.sort compare lst in
  let rec dedup lst =
    match lst with
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: rest -> if x = y then dedup (y :: rest) else x :: dedup (y :: rest)
  in
  dedup sorted
;;

module IntMap = Map.Make (struct
    type t = int

    let compare = compare
  end)

let list_to_map lst = List.fold_left (fun acc x -> IntMap.add x 0 acc) IntMap.empty lst
let update_map_key map key new_value = IntMap.add key new_value map

let part2 () =
  let left = ref [] in
  let right = ref [] in
  List.iter
    (fun ls ->
      let hd = List.hd ls in
      let last = List.nth ls 1 in
      left := hd :: !left;
      right := last :: !right)
    !all_pairs;
  left := unique !left;
  let left_map = ref (list_to_map !left) in
  let total_dis = ref 0 in
  List.iter
    (fun k ->
      let l =
        try IntMap.find k !left_map with
        | _ -> -1
      in
      if l == -1 then () else left_map := update_map_key !left_map k (l + 1))
    !right;
  IntMap.iter (fun i t -> total_dis := !total_dis + (i * t)) !left_map;
  print_endline @@ "result: " ^ string_of_int !total_dis
;;

(*  *)
let _ = part1 ()
let _ = part2 ()
