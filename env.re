module M = Map.Make(String);

open Types.MalType

let makeEnv = (env, binds, expr) => {
  let data = ref(M.empty);

  let obj = {
    pub set = (k: string, v: t) => data := M.add(k, v, data^);
    pub find = k =>
      switch (M.find_opt(k, data^), env) {
      | (Some(v), _) => Some(v)
      | (None, Some(env)) => env#find(k)
      | _ => None
      };
    pub get = k =>
      switch (this#find(k)) {
      | Some(v) => v
      | None => raise(Types.KeyNotFound("'" ++ k ++ "'" ++ " not found"))
      }
  };

  let rec set_helper = (binds, expr) =>
    switch (binds, expr) {
    | ([Symbol("&"), Symbol(key)], expr) =>
      obj#set(key, List(expr));
    | ([Symbol(key), ...rst_bindings], [expr, ...rst_exprs]) =>
      obj#set(key, expr);
      set_helper(rst_bindings, rst_exprs);
    | ([], _) => ()
    | (_, []) => raise(Failure("No expression for binding"))
    | (_, _) => raise(Failure("Internal error"))
    };

  set_helper(binds, expr);

  obj;
};
