let read = i => i;

let eval = i => i;

let print = i => i;

let rep = i => i |> read |> eval |> print;

let rec main = () => {
  print_string("user> ");
  switch (read_line()) {
  | input_line => input_line |> rep |> print_endline;
    main();
  | exception End_of_file =>  ()
  };
};

main();
