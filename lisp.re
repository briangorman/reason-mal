open Types;

module M = Map.Make(String);

exception Failure;

let numFun = (f, args) => {
  switch (args) {
  | [MalInteger(a), MalInteger(b)] => MalInteger(f(a, b))
  | _ => raise(Invalid_argument("Wrong args or function type"))
  };
};

let repl_env =
  M.empty
  |> M.add("+", MalFn(numFun((+))))
  |> M.add("-", MalFn(numFun((-))))
  |> M.add("/", MalFn(numFun((/))))
  |> M.add("*", MalFn(numFun(( * ))));


let read = str => Reader.read_str(str);

let rec eval = (ast, repl_env) => {
  switch (ast) {
  | MalList([]) => ast
  | MalList(_) =>
    switch (eval_ast(ast, repl_env)) {
    | MalList([MalFn(fn), ...args]) => fn(args)
    | _ => raise(Failure)
    }
  | MalFn(_) => raise(Failure);
  | MalInteger(_) => eval_ast(ast, repl_env)
  | MalSymbol(_) => eval_ast(ast, repl_env)
  };
}
and eval_ast = (ast, repl_env) => {
  switch (ast) {
  | MalSymbol(s) => M.find(s, repl_env)
  | MalList(lst) => MalList(List.map(ast => eval(ast, repl_env), lst))
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
