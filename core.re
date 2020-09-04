open Types.MalType;

let makeFn = Types.makeFn;

let numFun = (f, args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => Integer(f(a, b))
  | _ => raise(Failure("Wrong args or function type to numfun"))
  };
};

let str_helper = (~print_readably=false, args) => {
  switch (args) {
  | [] => raise(Failure("Wrong args or function type"))
  | lst =>
    String(
      lst |> List.map(Printer.pr_str(~print_readably)) |> String.concat(""),
    )
  };
};

let str = str_helper;
let pr_str = str_helper(~print_readably=true);

let listFn = args => {
  switch (args) {
  | toBeList => List(toBeList)
  };
};

let print_helper = (~print_readably, args) => {
  switch (args) {
  | lst =>
    lst
    |> List.map(Printer.pr_str(~print_readably))
    |> String.concat(" ")
    |> print_endline;
    Nil;
  };
};

let read_str = args => {
  switch (args) {
  | [String(str)] => str |> Reader.read_str
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let slurp = args => {
  switch (args) {
  | [String(fileName)] =>
    let b = Buffer.create(512);
    let channel = fileName |> open_in;
    let channelLength = in_channel_length(channel);
    Buffer.add_channel(b, channel, channelLength);
    String(Buffer.contents(b));
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let listQuestion = args => {
  switch (args) {
  | [List(_)] => True
  | _ => raise(Failure("Too many parameters passed to list?"))
  };
};

let listEmpty = args => {
  switch (args) {
  | [List([])] => True
  | [List(_some)] => False
  | _ => raise(Failure("Not a list!"))
  };
};

let count = args => {
  switch (args) {
  | [List([])] => Integer(0)
  | [Nil] => Integer(0)
  | [List(lst)]
  | [Vector(lst)] => Integer(List.length(lst))
  | _ => raise(Failure("count only works on lists or nil"))
  };
};

let first = args => {
  switch (args) {
  | [List([fst, ..._rst])]
  | [Vector([fst, ..._rst])] => fst
  | [List([])]
  | [Vector([])] => Nil
  | [Nil] => Nil
  | _ => raise(Failure("first only works on lists or vectors or nil"))
  };
};

let rest = args => {
  switch (args) {
  | [List([_fst, ...rst])]
  | [Vector([_fst, ...rst])] => List(rst)
  | [List([])]
  | [Vector([])]
  | [Nil] => List([])
  | _ => raise(Failure("rest only works on lists or vectors or nil"))
  };
};

let nth = args => {
  switch (args) {
  | [List(lst), Integer(n)]
  | [Vector(lst), Integer(n)] =>
    try(List.nth(lst, n)) {
    | _ => raise(Failure("Bounds error nth"))
    }
  | [Nil] => Nil
  | _ => raise(Failure("rest only works on lists or vectors or nil"))
  };
};

let cons = args => {
  switch (args) {
  | [arg1, List(lst)]
  | [arg1, Vector(lst)] => List([arg1] @ lst)
  | _ => raise(Failure("wrong arguments to cons"))
  };
};

let concat = args => {
  let rec acc = (accumulated, remaining) => {
    switch (remaining) {
    | [] => List(accumulated)
    | [List(lst), ...rst]
    | [Vector(lst), ...rst] => acc(accumulated @ lst, rst)
    | _ => raise(Failure("concat only works on lists"))
    };
  };
  acc([], args);
};

let vec = args => {
  switch (args) {
  | [List(lst)]
  | [Vector(lst)] => Vector(lst)
  | _ => raise(Failure("vec only works on lists and vectors"))
  };
};

type computationEnded =
  | EqualityComplete(Types.MalType.t)
  | InProgress(list(Types.MalType.t));

// Very Nice dual with apply
let rec equal = args => {
  switch (args) {
  | [Vector(lst), Vector(lst2)]
  | [List(lst), List(lst2)] =>
    switch (equal_list(lst, lst2)) {
    | EqualityComplete(complete) => complete
    | InProgress(lst) => equal(lst)
    }
  | [Integer(a), Integer(b)] => a == b ? True : False
  | [Symbol(s1), Symbol(s2)] => 0 == String.compare(s1, s2) ? True : False
  | [True, True] => True
  | [False, False] => True
  | [Nil, Nil] => True
  | [String(s1), String(s2)] => String.compare(s1, s2) == 0 ? True : False
  // This is missing support for Strings, Vectors and HashMaps
  | _ => False
  };
}
and equal_list = (lst1, lst2) =>
  switch (lst1, lst2) {
  | ([], []) => EqualityComplete(True)
  | ([_fst, ..._rst], []) => EqualityComplete(False)
  | ([], [_fst, ..._rst]) => EqualityComplete(False)
  | ([_fst1, ...rst1], [_fst2, ...rst2]) =>
    InProgress([List(rst1), List(rst2)])
  };

let intLessThan = args => {
  switch (args) {
  | [Integer(a), Integer(b)] => a < b ? True : False
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let intLessThanEqualTo = args => {
  switch (args) {
  | [Integer(a), Integer(b)] => a <= b ? True : False
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let mal_complement = tf => {
  switch (tf) {
  | True => False
  | False => True
  | _ => raise(Failure("bad"))
  };
};

let makeAtom = args => {
  switch (args) {
  // | [Atom(_)] => raise(Failure("Cannot make an atom of an atom"));
  | [mt] => Atom(ref(mt))
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let isAtom = args => {
  switch (args) {
  | [Atom(_)] => True
  | [_] => False
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let derefAtom = args => {
  switch (args) {
  | [Atom(mt)] => mt^
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let resetAtom = args => {
  switch (args) {
  | [Atom(r), newValue] =>
    r := newValue;
    newValue;
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let swapAtom = args => {
  switch (args) {
  | [Atom(mt), Fn(f, Function), ...fnArgs] =>
    let newValue = f([mt^] @ fnArgs);
    mt := newValue;
    newValue;
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let throw = args => {
  switch (args) {
  | [mt, ..._rest] => raise(Types.MalException(mt))
  | [] => raise(Failure("Must call throw with one arg"))
  };
};

let apply = args => {
  switch (args) {
  | [Fn(f, _), ...args] =>
    switch (List.rev(args)) {
    | [List(lastArgToApply), ...reversedArgsToFn]
    | [Vector(lastArgToApply), ...reversedArgsToFn] =>
      f(List.rev(reversedArgsToFn) @ lastArgToApply)
    | [] => f([])
    | _ => raise(Failure("last argument to apply must be coll if present"))
    }
  | _ => raise(Failure("apply must be called with a function"))
  };
};

let map = args => {
  switch (args) {
  | [Fn(f, _), Vector(mapArgs)]
  | [Fn(f, _), List(mapArgs)] => List(List.map(arg => f([arg]), mapArgs))
  | _ => raise(Failure("Invalid args to map"))
  };
};

let isNil = args => {
  switch (args) {
  | [Nil] => True
  | _ => False
  };
};

let isTrue = args => {
  switch (args) {
  | [True] => True
  | _ => False
  };
};

let typeOf = args => {
  switch (args) {
  | [Symbol(_)] => String("symbol")
  | [Vector(_)] => String("vector")
  | [List(_)] => String("list")
  | [HashMap(_)] => String("hashmap")
  | [Integer(_)] => String("integer")
  | [Fn(_, Function)] => String("function")
  | [Fn(_, Macro)] => String("macro")
  | [Nil] => String("nil")
  | [Atom(_)] => String("atom")
  | [True] => String("true")
  | [False] => String("false")
  | [String(_)] => String("string")
  | [Keyword(_)] => String("keyword")
  | _ => raise(Failure("wrong args to typeof"))
  };
};

let isSymbol = args => {
  switch (args) {
  | [Symbol(_)] => True
  | _ => False
  };
};

let ns = [
  ("+", makeFn(numFun((+)))),
  ("-", makeFn(numFun((-)))),
  ("/", makeFn(numFun((/)))),
  ("*", makeFn(numFun(( * )))),
  ("prn", makeFn(print_helper(~print_readably=true))),
  ("str", makeFn(str)),
  ("pr-str", makeFn(pr_str)),
  ("println", makeFn(print_helper(~print_readably=false))),
  ("vec", makeFn(vec)),
  ("list", makeFn(listFn)),
  ("list?", makeFn(listQuestion)),
  ("empty?", makeFn(listEmpty)),
  ("count", makeFn(count)),
  ("first", makeFn(first)),
  ("rest", makeFn(rest)),
  ("nth", makeFn(nth)),
  ("cons", makeFn(cons)),
  ("concat", makeFn(concat)),
  ("=", makeFn(equal)),
  ("<", makeFn(intLessThan)),
  ("<=", makeFn(intLessThanEqualTo)),
  (">", makeFn(x => x |> intLessThanEqualTo |> mal_complement)),
  (">=", makeFn(x => x |> intLessThan |> mal_complement)),
  ("slurp", makeFn(slurp)),
  ("read-string", makeFn(read_str)),
  ("atom", makeFn(makeAtom)),
  ("atom?", makeFn(isAtom)),
  ("deref", makeFn(derefAtom)),
  ("reset!", makeFn(resetAtom)),
  ("swap!", makeFn(swapAtom)),
  ("throw", makeFn(throw)),
  ("apply", makeFn(apply)),
  ("map", makeFn(map)),
  ("nil?", makeFn(isNil)),
  ("true?", makeFn(isTrue)),
  ("false?", makeFn(x => x |> isTrue |> mal_complement)),
  ("symbol?", makeFn(x => x |> isSymbol)),
];
