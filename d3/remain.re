module JaneStreet = {
  include Core;
  include Base;
};

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

let find_pattern = (regex, text) => {
  let rec aux = (start, acc) =>
    try({
      let pos = Str.search_forward(regex, text, start);
      let matched = Str.matched_string(text);
      aux(pos + 1, [matched, ...acc]);
    }) {
    /* Add the match to the accumulator */

    | Not_found => List.rev(acc)
    }; /* Reverse the list before returning */

  aux(0, []);
};

let all_lines = ref([]);

let load_input = filepath => {
  let count = ref(1);
  consume_input_text_line_by_line(
    ~filepath,
    ~consume=line => {
      all_lines := [line, ...all_lines^];
      count := count^ + 1;
    },
  );
};

let _ = load_input("./input");

let get_product = s => {
  let s_lst = Str.split(Str.regexp({|mul(\|,\|)|}), s);
  let s_lst2 = List.filter(s => s !== "", s_lst);
  let i_list = List.map(s => int_of_string(s), s_lst2);
  let product = List.fold_left((acc, i) => acc * i, 1, i_list);
  product;
};

/*  */
let part1 = () => {
  let long_text =
    List.fold_left((acc, l) => acc ++ l, "", List.rev(all_lines^));
  print_endline @@ "long_text: " ++ long_text;
  print_endline @@ "long_text ends";
  let all_mul =
    find_pattern(
      Str.regexp({|mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)|}),
      long_text,
    );

  List.iter(mul => print_endline @@ "mul: " ++ mul, all_mul);
  let all_products = List.map(m => get_product(m), all_mul);
  let sum_of_all_products =
    List.fold_left((acc, p) => acc + p, 0, all_products);
  print_endline @@ "result: " ++ string_of_int(sum_of_all_products);
};

let part2 = () => {
  let long_text =
    List.fold_left((acc, l) => acc ++ l, "", List.rev(all_lines^));
  let instructions =
    find_pattern(
      Str.regexp({|mul([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?)\|do()\|don't()|}),
      long_text,
    );

  List.iter(i => print_endline @@ "instruction: " ++ i, instructions);
  let state = ref(true);
  let all_products =
    List.map(
      s =>
        switch (s) {
        | "do()" =>
          state := true;
          0;
        | "don't()" =>
          state := false;
          0;
        | m =>
          if (state^) {
            get_product(m);
          } else {
            0;
          }
        },
      instructions,
    );

  let sum_of_all_products =
    List.fold_left((acc, p) => acc + p, 0, all_products);
  print_endline @@ "result: " ++ string_of_int(sum_of_all_products);
};

/*  */
let _ = part1();
let _ = part2();

/* Function to find all occurrences of a pattern in a text */
