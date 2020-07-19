type malType =
  | MalList(list(malType))
  | MalSymbol(string)
  | MalInteger(int)

