module M = Map.Make(String);

exception KeyNotFound(string);

let makeEnv = env => {
  let data = ref(M.empty);
  {
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
      | None => raise(KeyNotFound(k))
      }
  };
};
