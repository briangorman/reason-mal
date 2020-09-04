exception KeyNotFound(string);

module rec MalType: {
  type fnType =
    | Function
    | Macro;

  type t =
    | List(list(t))
    | Vector(list(t))
    | Symbol(string)
    | Keyword(string)
    | String(string)
    | Integer(int)
    | HashMap(MalMap.t(t))
    | Fn(list(t) => t, fnType)
    | Atom(ref(t))
    | Nil
    | True
    | False;

} = MalType
and
  MalComparable: {
  type t = MalType.t;
  let compare: (t,t) => int;
}  = {
  type t = MalType.t;
  let compare = Stdlib.compare
}
and MalMap: Map.S with type key = MalComparable.t = Map.Make(MalComparable);

exception MalException(MalType.t);

let makeFn = x => MalType.Fn(x, Function);
