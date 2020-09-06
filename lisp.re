open Types.MalType;

module M = Types.MalMap;

let repl_env = Env.makeEnv(None, [], []);

let numFun = (f, args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => Integer(f(a, b))
  | _ => raise(Failure("Wrong args or function type"))
  };
};

List.iter(((k, v)) => repl_env#set(k, v), Core.ns);

let read = str => Reader.read_str(str);

let rec macroexpand = (ast, env) => {
  switch (ast) {
  | List([Symbol(k), ...rst]) =>
    switch (env#get(k)) {
    | Fn(fn, Macro) => macroexpand(fn(rst), env)
    | exception (Types.KeyNotFound(_)) => ast // Catch key not found in macroexpand to prevent functions defined by def from blowing up at interpreter startup
    | _ => ast
    }
  | _ => ast
  };
};

let rec quasiquote = ast => {
  switch (ast) {
  | List([Symbol("unquote"), second]) => second
  | List([List([Symbol("splice-unquote"), snd]), ...rst]) =>
    List([Symbol("concat"), snd, quasiquote(List(rst))])
  | List([fst, ...rst]) =>
    List([Symbol("cons"), quasiquote(fst), quasiquote(List(rst))])
  | HashMap(_) => List([Symbol("quote"), ast])
  | Symbol(_) => List([Symbol("quote"), ast])
  | Vector(lst) => List([Symbol("vec"), quasiquote(List(lst))])
  | _ => ast
  };
};

let rec eval = (ast, repl_env) => {
  let ast = macroexpand(ast, repl_env);
  switch (ast) {
  | List([]) => ast
  | List([Symbol("try*"), a, List([Symbol("catch*"), b, c])]) =>
    try(eval(a, repl_env)) {
    | Types.MalException(mt) =>
      let newEnv = Env.makeEnv(Some(repl_env), [b], [mt]);
      eval(c, newEnv);
    | Failure(s)
    | Types.KeyNotFound(s)
    | Invalid_argument(s) =>
      // Typing into bindings could be improved
      let newEnv = Env.makeEnv(Some(repl_env), [b], [String(s)]);
      eval(c, newEnv);
    }
  | List([Symbol("quote"), arg]) => arg
  | List([Symbol("macroexpand"), arg]) => macroexpand(arg, repl_env)
  | List([Symbol("quasiquoteexpand"), arg]) => quasiquote(arg)
  | List([Symbol("quasiquote"), arg]) => eval(quasiquote(arg), repl_env) // Todo flip args of eval
  | List([Symbol("def!"), Symbol(k), expr]) =>
    let value = eval(expr, repl_env);
    repl_env#set(k, value);
    value;
  | List([Symbol("defmacro!"), Symbol(k), expr]) =>
    let value =
      switch (eval(expr, repl_env)) {
      | Fn(fn, Function) => Fn(fn, Macro)
      | _ => raise(Failure("Input to defmacro must be a function"))
      };
    repl_env#set(k, value);
    value;

  | List([Symbol("let*"), List(bindings), body])
  | List([Symbol("let*"), Vector(bindings), body]) =>
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
  | List([Symbol("fn*"), Vector(bindings), body])
  | List([Symbol("fn*"), List(bindings), body]) =>
    Types.makeFn(args =>
      eval(body, Env.makeEnv(Some(repl_env), bindings, args))
    )

  | List(_) =>
    switch (eval_ast(ast, repl_env)) {
    | List([Fn(fn, Function), ...args]) => fn(args)

    | _ => raise(Failure("Function not in first position in apply phase"))
    }
  | ast => eval_ast(ast, repl_env)
  };
}
and eval_ast = (ast, repl_env) => {
  switch (ast) {
  | Symbol(s) => repl_env#get(s)
  | List(lst) => List(List.map(ast => eval(ast, repl_env), lst))
  | Vector(lst) => Vector(List.map(ast => eval(ast, repl_env), lst))
  | HashMap(hm) => HashMap(M.map(ast => eval(ast, repl_env), hm))
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

let eval_fn = args => {
  switch (args) {
  | [ast] => eval(ast, repl_env)
  | _ => raise(Failure("Wrong args or function type"))
  };
};

repl_env#set("eval", Types.makeFn(eval_fn));

let print = form => Printer.pr_str(~print_readably=true, form);

// Todo, ocamlize the function signatures to allow for |>
let rep = str => print(eval(read(str), repl_env));

let loadFileDefinition = "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"nil)\")))))";
rep(loadFileDefinition);
rep("(def! not (fn* (a) (if a false true)))");

rep(
  "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
);

let argLength = Array.length(Sys.argv);

if (argLength > 1) {
  let argv =
    Array.sub(Sys.argv, 2, argLength - 2)
    |> Array.fold_left((acc, next) => acc @ [String(next)], []);
  repl_env#set("*ARGV*", List(argv));
  rep("(load-file \"" ++ Sys.argv[1] ++ "\")") |> ignore;
} else {
  rep("(println (str \"Mal [\" *host-language* \"]\"))") |> ignore;
  let rec main = () => {
    print_string("user> ");
    switch (read_line()) {
    | input_line =>
      try(input_line |> rep |> print_endline) {
      | Types.KeyNotFound(s)
      | Invalid_argument(s)
      | Failure(s) => print_endline(s)
      | Types.MalException(mt) =>
        print_endline(
          "Uncaught mal exception: "
          ++ Printer.pr_str(~print_readably=true, mt),
        )
      | Stdlib.Scanf.Scan_failure(s) => print_endline(s)
      | _ => print_endline("Unhandled exception from within....")
      };
      main();
    | exception End_of_file => ()
    };
  };

  main();
};
