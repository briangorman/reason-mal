exception KeyNotFound(string);
exception Failure(string);

module StringMap = Map.Make(String);

type malType =
  | List(list(malType))
  | Vector(list(malType))
  | Symbol(string)
  | Keyword(string)
  | String(string)
  | Integer(int)
  | HashMap(StringMap.t(malType))
  | Fn(list(malType) => malType)
  | Nil
  | True
  | False;
