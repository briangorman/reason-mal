exception KeyNotFound(string);
exception Failure(string);

module StringMap = Map.Make(String);

type fnType =
  | Function
  | Macro;

type malType =
  | List(list(malType))
  | Vector(list(malType))
  | Symbol(string)
  | Keyword(string)
  | String(string)
  | Integer(int)
  | HashMap(StringMap.t(malType))
  | Fn(list(malType) => malType, fnType)
  | Atom(ref(malType))
  | Nil
  | True
  | False;

let makeFn = x => Fn(x, Function);
