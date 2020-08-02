exception KeyNotFound(string);
exception Failure(string);

type malType =
  | List(list(malType))
  | Vector(list(malType))
  | Symbol(string)
  | Integer(int)
  | Fn(list(malType) => malType)
  | Nil
  | True
  | False;
