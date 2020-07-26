open Types;

exception Failure(string);

let numFun = (f, args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => Integer(f(a, b))
  | _ => raise(Invalid_argument("Wrong args or function type"))
  };
};

let repl_env = Env.makeEnv(None);

repl_env#set("+", Fn(numFun((+))));
repl_env#set("-", Fn(numFun((-))));
repl_env#set("/", Fn(numFun((/))));
repl_env#set("*", Fn(numFun(( * ))));

let read = str => Reader.read_str(str);

let rec eval = (ast, repl_env) => {
  switch (ast) {
  | List([]) => ast
  | List([Symbol("def!"), Symbol(k), expr]) =>
    let value = eval(expr, repl_env);
    repl_env#set(k, value);
    value;
  | List([Symbol("let*"), List(bindings), body]) =>
    let newEnv = createEnvWithBindings(bindings, repl_env);
    eval(body, newEnv);
  | List(_) =>
    switch (eval_ast(ast, repl_env)) {
    | List([Fn(fn), ...args]) => fn(args)
    | _ => raise(Failure("Function not in first position in apply phase"))
    }
  | _ => eval_ast(ast, repl_env)
  };
}
and eval_ast = (ast, repl_env) => {
  switch (ast) {
  | Symbol(s) => repl_env#get(s)
  | List(lst) => List(List.map(ast => eval(ast, repl_env), lst))
  | _ => ast
  };
}
and createEnvWithBindings = (bindings, oldEnv) => {
  let newEnv = Env.makeEnv(Some(oldEnv));

  let rec acc = lst =>
    switch (lst) {
    | [Symbol(key), expr, ...rest] =>
      newEnv#set(key, eval(expr, newEnv));
      acc(rest);
    | [] => ()
    | _ => raise(Failure("let* bindings require an even number of forms"))
    };
  acc(bindings);
  newEnv;
};

let print = form => Printer.pr_str(form);

let rep = str => print(eval(read(str), repl_env));

let rec main = () => {
  print_string("user> ");
  switch (read_line()) {
  | input_line =>
    try(input_line |> rep |> print_endline) {
    | Env.KeyNotFound(key) => print_endline(key ++ " not found")
    | Failure(s) => print_endline(s)
    | Invalid_argument(s) => print_endline(s)
    };
    main();
  | exception End_of_file => ()
  };
};

main();
