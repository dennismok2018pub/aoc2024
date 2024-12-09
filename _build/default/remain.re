module JaneStreet = {
  include Core;
};
/* let consume_input_text_line_by_line ~filepath ~consume =
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
   let _ = run () */

/*  */

/* p2 */

let consume_input_text_line_by_line = (~filepath, ~consume) => {
  let rec exhaust = channel =>
    switch (In_channel.input_line(channel)) {
    | None => ()
    | Some(line) =>
      consume(line);
      exhaust(channel);
    };

  In_channel.with_open_text(filepath, channel => exhaust(channel));
};

/* let replace a ind lst = List.mapi (fun i e -> if i = ind then a else e) lst */

/* let left_and_right_consume_input_text_line_by_line
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
   ;; */

/* let unique lst =
   let rec aux l acc =
   match l with
   | [] -> acc
   | h :: t -> if List.mem h acc then aux t acc else aux t (h :: acc)
   in
   aux lst []
   ;; */
/* todo */

let extract_numerics_from_line = (line, i) => {
  print_endline @@ "line#" ++ string_of_int(i) ++ ":" ++ line;
  let strs = Str.split(Str.regexp("   "), line);
  List.map(int_of_string, strs);
};

let all_pairs = ref([]);

let load_input = filepath => {
  let count = ref(1);
  consume_input_text_line_by_line(
    ~filepath,
    ~consume=line => {
      all_pairs := [extract_numerics_from_line(line, count^), ...all_pairs^];
      count := count^ + 1;
    },
  );
};


/*  */
let run = () => {
  let left = ref([]);
  let right = ref([]);
  load_input("./input");
  List.iter(
    ls => {
      let hd = List.hd(ls);
      let last = List.nth(ls, 1);
      left := [hd, ...left^];
      right := [last, ...right^];
    },
    all_pairs^,
  );
  left := List.sort(compare, left^);
  right := List.sort(compare, right^);
  let total_dis = ref(0);
  List.iter(
    _ => {
      let l = List.hd(left^);
      let r = List.hd(right^);
      total_dis := total_dis^ + abs(l - r);
      left := List.tl(left^);
      right := List.tl(right^);
    },
    List.init(List.length(left^), i => i),
  );
  print_endline @@ "result: " ++ string_of_int(total_dis^);
};

/*  */
let _ = run();
