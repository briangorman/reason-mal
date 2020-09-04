

module M = Types.MalMap
open Types.MalType

let rec pr_str = (~print_readably=false, form) =>
  switch (form) {
  | Integer(i) => string_of_int(i)
  | Symbol(s) => s
  | Keyword(s) => s
  | List(lst) =>
    ["(", List.map(pr_str(~print_readably), lst) |> String.concat(" "), ")"]
    |> String.concat("")
  | Vector(lst) =>
    ["[", List.map(pr_str(~print_readably), lst) |> String.concat(" "), "]"]
    |> String.concat("")
  | HashMap(hm) =>
    [
      "{",
      M.bindings(hm)
      |> List.map(tup => pr_str_hash_tuple(~print_readably, tup))
      |> String.concat(" "),
      "}",
    ]
    |> String.concat("")
  | Fn(_) => "#<function>"
  | Nil => "nil"
  | False => "false"
  | True => "true"
  | Atom(_) => "#<atom>"
  | String(s) =>  print_readably ? "\"" ++ (String.escaped(s)) ++ "\"" : s
  }
and pr_str_hash_tuple = (~print_readably=false, (k, v)) => {
  pr_str(~print_readably,k) ++ " " ++ pr_str(~print_readably, v);
};
