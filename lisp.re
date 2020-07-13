//open Types;

let read = str => Reader.read_str(str)

let eval = i => i;

let print = form => Printer.pr_str(form);

let rep = i => i |> read |> eval |> print;

let rec main = () => {
  print_string("user> ");
  switch (read_line()) {
  | input_line =>
    input_line |> rep |> print_endline;
    main();
  | exception End_of_file => ()
  };
};

main();
