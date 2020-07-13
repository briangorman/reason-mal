type malPrimative =
  | MalSymbol(string)
  | MalInteger(int)


type malType =
  | MalList(list(malType))
  | MalAtom(malPrimative);

