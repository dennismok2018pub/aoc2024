module JaneStreet = {
  include Core;
  include Base;
};

let consume_input_text_line_by_line = (~filepath, ~consume) => {
  let rec exhaust = channel =>
    switch (In_channel.input_line(channel)) {
    | None => ()
    | Some(line) =>
      /* print_endline @@ "reading: " ^ line; */
      consume(line);
      exhaust(channel);
    };

  In_channel.with_open_text(filepath, channel => exhaust(channel));
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

/* let _ = load_input "./input2" */

let _ = load_input("./input");
let rules = ref([]);
let updates = ref([]);

let () =
  List.iter(
    l =>
      if (String.contains(l, '|')) {
        print_endline @@ "adding to rules: " ++ l;
        rules := [l, ...rules^];
      } else if (String.contains(l, ',')) {
        /* print_endline @@ "adding to updates: " ^ l; */
        updates := [l, ...updates^];
      },
    List.rev(all_lines^),
  );

let clear_line_with_rule = (line, rule) => {
  let rule_split = String.split_on_char('|', rule);
  let line_split = String.split_on_char(',', line);
  print_endline @@ "clearing with rule: " ++ rule;
  print_endline @@ "clearing with line: " ++ line;
  let left =
    List.fold_left(
      (acc, s) =>
        if (List.is_empty(acc) === false) {
          print_endline @@ "matching: " ++ List.hd(acc) ++ " & " ++ s;
          if (String.equal(List.hd(acc), s)) {
            List.tl(acc);
          } else {
            acc;
          };
        } else {
          acc;
        },
      rule_split,
      line_split,
    );

  let res =
    if (List.is_empty(left) || List.length(left) === 2) {
      1;
    } else if (None
               === List.find_opt(
                     s => String.equal(s, List.hd(left)),
                     line_split,
                   )) {
      1;
    } else {
      0;
    };

  print_endline @@ "res:" ++ string_of_int(res);
  res;
};

let clear_line_with_rules = (line, rs) =>
  List.fold_left(
    (acc, r) => {
      print_endline @@ "rule: " ++ r;
      acc * clear_line_with_rule(line, r);
    },
    1,
    rs,
  );

/*  */
let part1 = () => {
  let valid_lines = ref([]);
  let sum_of_middles = ref(0);
  List.iter(
    update =>
      if (clear_line_with_rules(update, rules^) === 1) {
        valid_lines := [update, ...valid_lines^];
      },
    updates^,
  );
  List.iter(
    line => {
      let l = String.split_on_char(',', line);
      let middle = int_of_string @@ List.nth(l, List.length(l) / 2);
      sum_of_middles := sum_of_middles^ + middle;
    },
    valid_lines^,
  );
  print_endline @@ "part1 result: " ++ string_of_int(sum_of_middles^);
};

let part2 = () => {
  /* let valid_lines = ref [] in */
  let sum_of_middles = ref(0);
  print_endline @@ "part2 result: " ++ string_of_int(sum_of_middles^);
};

/*  */
let _ = part1();
let _ = part2();

/* Function to find all occurrences of a pattern in a text */
