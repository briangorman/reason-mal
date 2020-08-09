open Types;

let repl_env = Env.makeEnv(None, [], []);

let numFun = (f, args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => Integer(f(a, b))
  | _ => raise(Failure("Wrong args or function type"))
  };
};

List.iter(((k, v)) => repl_env#set(k, v), Core.ns);

let read = str => Reader.read_str(str);

let rec eval = (ast, repl_env) => {
  switch (ast) {
  | List([]) => ast
  | List([Symbol("def!"), Symbol(k), expr]) =>
    let value = eval(expr, repl_env);
    repl_env#set(k, value);
    value;
  | List([Symbol("let*"), List(bindings), body])
  | Vector([Symbol("let*"), List(bindings), body]) =>
    let newEnv = createEnvWithBindings(bindings, repl_env);
    eval(body, newEnv);
  | List([Symbol("do"), ...body]) =>
    List.fold_left((_acc, next) => eval(next, repl_env), Nil, body)
  | List([Symbol("if"), conditional, then_, else_]) =>
    switch (eval(conditional, repl_env)) {
    | False
    | Nil => eval(else_, repl_env)
    | _ => eval(then_, repl_env)
    }
  | List([Symbol("if"), conditional, then_]) =>
    switch (eval(conditional, repl_env)) {
    | False
    | Nil => Nil
    | _ => eval(then_, repl_env)
    }
  | List([Symbol("fn*"), List(bindings), body]) =>
    Fn(args => eval(body, Env.makeEnv(Some(repl_env), bindings, args)))

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
  | Vector(lst) => Vector(List.map(ast => eval(ast, repl_env), lst))
  | HashMap(hm) => HashMap(StringMap.map(ast => eval(ast, repl_env), hm))
  | _ => ast
  };
}
and createEnvWithBindings = (bindings, oldEnv) => {
  // Initial function created to support let bindings....
  // Look into replacing this asap
  let newEnv = Env.makeEnv(Some(oldEnv), [], []);

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

let print = form => Printer.pr_str(~print_readably=true, form);

// Todo, ocamlize the function signatures to allow for |>
let rep = str => print(eval(read(str), repl_env));

let rec main = () => {
  print_string("user> ");
  switch (read_line()) {
  | input_line =>
    try(input_line |> rep |> print_endline) {
    | KeyNotFound(key) => print_endline(key ++ " not found")
    | Failure(s) => print_endline(s)
    | Stdlib.Scanf.Scan_failure(s) => print_endline(s)
    | _ => print_endline("Unhandled exception from within....")
    };
    main();
  | exception End_of_file => ()
  };
};

main();
