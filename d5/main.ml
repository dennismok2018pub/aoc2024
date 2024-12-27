module JaneStreet = struct
  include Core
  include Base
end

let consume_input_text_line_by_line ~filepath ~consume =
  let rec exhaust channel =
    match In_channel.input_line channel with
    | None -> ()
    | Some line ->
      (* print_endline @@ "reading: " ^ line; *)
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

(* let _ = load_input "./input2" *)

let _ = load_input "./input"
let rules = ref []
let updates = ref []

let () =
  List.iter
    (fun l ->
      if String.contains l '|'
      then (* print_endline @@ "adding to rules: " ^ l; *)
        rules := l :: !rules
      else if String.contains l ','
      then (* print_endline @@ "adding to updates: " ^ l; *)
        updates := l :: !updates)
    (List.rev !all_lines)
;;

let clear_line_with_rule line rule =
  let rule_split = String.split_on_char '|' rule in
  let line_split = String.split_on_char ',' line in
  (* print_endline @@ "clearing with rule: " ^ rule;
  print_endline @@ "clearing with line: " ^ line; *)
  let left =
    List.fold_left
      (fun acc s ->
        if List.is_empty acc == false
        then
          if (* print_endline @@ "matching: " ^ List.hd acc ^ " & " ^ s; *)
             String.equal (List.hd acc) s
          then List.tl acc
          else acc
        else acc)
      rule_split
      line_split
  in
  let res =
    if List.is_empty left || List.length left == 2
    then 1
    else if None == List.find_opt (fun s -> String.equal s (List.hd left)) line_split
    then 1
    else 0
  in
  (* print_endline @@ "res:" ^ string_of_int res; *)
  res
;;

let clear_line_with_rules line rs =
  List.fold_left
    (fun acc r ->
      (* print_endline @@ "rule: " ^ r; *)
      acc * clear_line_with_rule line r)
    1
    rs
;;

(*  *)
let part1 () =
  let valid_lines = ref [] in
  let sum_of_middles = ref 0 in
  List.iter
    (fun update ->
      if clear_line_with_rules update !rules == 1
      then valid_lines := update :: !valid_lines)
    !updates;
  List.iter
    (fun line ->
      let l = String.split_on_char ',' line in
      let middle = int_of_string @@ List.nth l (List.length l / 2) in
      sum_of_middles := !sum_of_middles + middle)
    !valid_lines;
  print_endline @@ "part1 result: " ^ string_of_int !sum_of_middles
;;

module IntSet = Set.Make (Int)
module IntMap = Map.Make (Int)

let get_rule_pairs string_lst =
  let str_pair_lst = List.map (fun s -> String.split_on_char '|' s) string_lst in
  List.map
    (fun ls -> int_of_string @@ List.hd ls, int_of_string @@ List.hd @@ List.tl ls)
    str_pair_lst
;;

(* Function to build adjacency list from a list of directed pairs *)
let build_adjacency_list pairs =
  let adjacency_list = ref IntMap.empty in
  (* Helper function to add an edge to the adjacency list *)
  let add_edge u v =
    adjacency_list
    := IntMap.update
         u
         (function
           | None -> Some [ v ] (* If no entry, create a new list *)
           | Some neighbors -> Some (v :: neighbors))
         !adjacency_list
  in
  (* Process each pair to build the adjacency list *)
  List.iter
    (fun (u, v) ->
      add_edge u v;
      (* Ensure both vertices are represented in the map *)
      if not (IntMap.mem u !adjacency_list)
      then adjacency_list := IntMap.add u [] !adjacency_list;
      if not (IntMap.mem v !adjacency_list)
      then adjacency_list := IntMap.add v [] !adjacency_list)
    pairs;
  !adjacency_list
;;

(* Function to print the adjacency list for clarity *)
let _print_adjacency_list adjacency_list =
  IntMap.iter
    (fun key value ->
      Printf.printf "%d -> [%s]\n" key (String.concat "; " (List.map string_of_int value)))
    adjacency_list
;;

(* Example usage *)
(* let () =
   let pairs = [(1, 2); (1, 3); (2, 4); (3, 4)] in
   let adjacency_list = build_adjacency_list pairs in
   print_adjacency_list adjacency_list *)

(* Function to perform DFS and find topological order with cycle detection *)
let rec dfs vertex visited stack recursion_stack adjacency_list =
  if not (None == List.find_opt (fun v -> v == vertex) !recursion_stack)
  then
    raise
      (Failure
         ("Graph has a cycle involving: "
          ^ String.concat " -> " (List.map string_of_int @@ List.rev !recursion_stack)
          ^ " -> "
          ^ string_of_int vertex))
  else if not (IntSet.mem vertex !visited)
  then (
    (* Add to recursion stack *)
    recursion_stack := vertex :: !recursion_stack;
    (* Mark the vertex as visited *)
    visited := IntSet.add vertex !visited;
    (* Visit all the adjacent vertices *)
    let neighbors = IntMap.find vertex adjacency_list in
    List.iter
      (fun neighbor -> dfs neighbor visited stack recursion_stack adjacency_list)
      neighbors;
    recursion_stack := List.tl !recursion_stack;
    (* Remove from recursion stack *)

    (* Push the vertex onto the stack after visiting all its neighbors *)
    stack := vertex :: !stack)
;;

(* Function to perform topological sort *)
let topological_sort adjacency_list =
  let visited = ref IntSet.empty in
  let stack = ref [] in
  let recursion_stack = ref [] in
  IntMap.iter
    (fun vertex _ ->
      if not (IntSet.mem vertex !visited)
      then dfs vertex visited stack recursion_stack adjacency_list)
    adjacency_list;
  List.rev !stack (* Return the reversed stack as the topological order *)
;;

(* Function to print the topological order *)

let print_result result =
  match result with
  | Ok order ->
    Printf.printf
      "Topological Order: [%s]\n"
      (String.concat "; " (List.map string_of_int order))
  | Error msg -> Printf.printf "Error: %s\n" msg
;;

let get_all_ints_from_an_invalid_line str =
  let str_pair_lst = String.split_on_char ',' str in
  List.map (fun s -> int_of_string s) str_pair_lst
;;

let get_relevant_rules int_lst pairs =
  List.filter
    (fun pair ->
      let l, _r = pair in
      match List.find_opt (fun ll -> l == ll) int_lst with
      | None -> false
      | _ -> true)
    pairs
;;

let reorder int_lst top_order =
  let reorder_lst = ref [] in
  List.iter
    (fun appearance ->
      match List.find_opt (fun ii -> ii = appearance) int_lst with
      | None -> ()
      | Some _ -> reorder_lst := appearance :: !reorder_lst)
    top_order;
  !reorder_lst
;;

let part2 () =
  let invalid_lines = ref [] in
  let sum_of_middles = ref 0 in
  List.iter
    (fun update ->
      if false == (clear_line_with_rules update !rules == 1)
      then invalid_lines := update :: !invalid_lines)
    !updates;
  let pairs = get_rule_pairs !rules in
  List.iter
    (fun invalid_line ->
      print_endline "---------------";
      let int_lst = get_all_ints_from_an_invalid_line invalid_line in
      let relevant_rules = get_relevant_rules int_lst pairs in
      let adjacency_list = build_adjacency_list relevant_rules in
      (* print_adjacency_list adjacency_list; *)
      try
        let top_order = topological_sort adjacency_list in
        print_result (Ok top_order);
        let re_order = reorder int_lst top_order in
        Printf.printf
          "Original Order: [%s]\n"
          (String.concat "; " (List.map string_of_int int_lst));
        Printf.printf
          "Re-Order: [%s]\n"
          (String.concat "; " (List.map string_of_int re_order));
        let middle = List.nth re_order (List.length re_order / 2) in
        sum_of_middles := !sum_of_middles + middle
      with
      | Failure msg -> print_result (Error msg))
    !invalid_lines;
  print_endline @@ "part2 result: " ^ string_of_int !sum_of_middles
;;

(*  *)
let _ = part1 ()
let _ = part2 ()
