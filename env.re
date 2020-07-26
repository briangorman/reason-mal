module M = Map.Make(String);

open Types;

let makeEnv = (env, binds, expr) => {
  let data = ref(M.empty);

  let obj = {
    pub set = (k: string, v: Types.malType) => data := M.add(k, v, data^);
    pub find = k =>
      switch (M.find_opt(k, data^), env) {
      | (Some(v), _) => Some(v)
      | (None, Some(env)) => env#find(k)
      | _ => None
      };
    pub get = k =>
      switch (this#find(k)) {
      | Some(v) => v
      | None => raise(Types.KeyNotFound(k))
      }
  };

  List.combine(binds, expr)
  |> List.iter(((binding, expr)) => switch(binding, expr) { | (Symbol(key), expr) => obj#set(key, expr) });

  obj;
};
