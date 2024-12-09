module JaneStreet = {
  include Core;
  include Base;
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

let extract_numerics_from_line = (delitmiter, line, i) => {
  print_endline @@ "line#" ++ string_of_int(i) ++ ":" ++ line;
  let strs = Str.split(Str.regexp(delitmiter), line);
  List.map(int_of_string, strs);
};

let all_lines = ref([]);

let load_input = filepath => {
  let count = ref(1);
  consume_input_text_line_by_line(
    ~filepath,
    ~consume=line => {
      all_lines :=
        [extract_numerics_from_line(" ", line, count^), ...all_lines^];
      count := count^ + 1;
    },
  );
};

let _ = load_input("./input");

let check_safe = lst => {
  let safe = ref(true);
  let direction = ref([]);
  let last = ref([]);
  List.iter(
    t =>
      if (safe^ === false) {
        ();
      } else if (last^ === []) {
        last := [t];
      } else if (direction^ === []) {
        let a = abs(t - List.hd(last^));
        if (a > 3 || a < 1) {
          safe := false;
        };
        let cur_dir = compare(t, List.hd(last^));
        direction := [cur_dir];
        last := [t];
        if (0 === List.hd(direction^)) {
          safe := false;
        };
      } else {
        let a = abs(t - List.hd(last^));
        if (a > 3 || a < 1) {
          safe := false;
        };
        let last_direction = List.hd(direction^);
        let current_direction = compare(t, List.hd(last^));
        last := [t];
        if (last_direction !== current_direction) {
          safe := false;
        } else {
          ();
        };
      },
    lst,
  );
  safe^;
};

/*  */
let part1 = () => {
  let safe_count = ref(0);
  List.iter(
    l =>
      if (check_safe(l)) {
        safe_count := safe_count^ + 1;
      } else {
        ();
      },
    all_lines^,
  );
  print_endline @@ "result: " ++ string_of_int(safe_count^);
};

let part2 = () => ();

/*  */
let _ = part1();
let _ = part2();
