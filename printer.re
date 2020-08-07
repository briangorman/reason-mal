open Types;

let rec pr_str = (~print_readably=false, form) =>
  switch (form) {
  | Integer(i) => string_of_int(i)
  | Symbol(s) => s
  | Keyword(s) => s
  | List(lst) =>
    ["(", List.map(pr_str, lst) |> String.concat(" "), ")"]
    |> String.concat("")
  | Vector(lst) =>
    ["[", List.map(pr_str, lst) |> String.concat(" "), "]"]
    |> String.concat("")
  | HashMap(hm) =>
    [
      "{",
      StringMap.bindings(hm)
      |> List.map(pr_str_hash_tuple)
      |> String.concat(" "),
      "}",
    ]
    |> String.concat("")
  | Fn(_) => "#<function>"
  | Nil => "nil"
  | False => "false"
  | True => "true"
  | String(s) =>  print_readably ? String.escaped(s) : s
  }
and pr_str_hash_tuple = ((k, v)) => {
  k ++ " " ++ pr_str(v);
};
