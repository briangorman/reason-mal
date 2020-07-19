open Types;

module M = Map.Make(String);

exception Failure;

let numFun = (f, args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => Integer(f(a, b))
  | _ => raise(Invalid_argument("Wrong args or function type"))
  };
};

let repl_env =
  M.empty
  |> M.add("+", Fn(numFun((+))))
  |> M.add("-", Fn(numFun((-))))
  |> M.add("/", Fn(numFun((/))))
  |> M.add("*", Fn(numFun(( * ))));


let read = str => Reader.read_str(str);

let rec eval = (ast, repl_env) => {
  switch (ast) {
  | List([]) => ast
  | List(_) =>
    switch (eval_ast(ast, repl_env)) {
    | List([Fn(fn), ...args]) => fn(args)
    | _ => raise(Failure)
    }
  | Fn(_) => raise(Failure);
  | Integer(_) => eval_ast(ast, repl_env)
  | Symbol(_) => eval_ast(ast, repl_env)
  };
}
and eval_ast = (ast, repl_env) => {
  switch (ast) {
  | Symbol(s) => M.find(s, repl_env)
  | List(lst) => List(List.map(ast => eval(ast, repl_env), lst))
  | _ => ast
  };
};

let print = form => Printer.pr_str(form);

let rep = str => print(eval(read(str), repl_env));

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
