open Types.MalType;
module MalMap = Types.MalMap;

let makeFn = Types.makeFn;

let numFun = (f, args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => Integer(f(a, b))
  | _ => raise(Failure("Wrong args or function type to numfun"))
  };
};

let str_helper = (~print_readably=false, seperator, args) => {
  switch (args) {
  | [] => String("")
  | lst =>
    String(
      lst
      |> List.map(Printer.pr_str(~print_readably))
      |> String.concat(seperator),
    )
  };
};

let str = str_helper("");
let pr_str = str_helper(~print_readably=true, " ");

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

let isList = args => {
  switch (args) {
  | [List(_)] => True
  | _ => False
  };
};

let listEmpty = args => {
  switch (args) {
  | [Vector([])]
  | [List([])] => True
  | [Vector(_some)]
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

let rec equal = args => {
  switch (args) {
  | [Vector(lst), Vector(lst2)]
  | [List(lst), Vector(lst2)]
  | [Vector(lst), List(lst2)]
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
  | [Keyword(s1), Keyword(s2)]
  | [String(s1), String(s2)] => String.compare(s1, s2) == 0 ? True : False
  | [HashMap(m1), HashMap(m2)] =>
    // This could be done better
    MalMap.equal(
      (v1, v2) =>
        switch (equal([v1, v2])) {
        | True => true
        | _ => false
        },
      m1,
      m2,
    )
      ? True : False
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

let symbol = args => {
  switch (args) {
  | [String(s)] => Symbol(s)
  | _ => raise(Failure("wrong args to symbol"))
  };
};

let keyword = args => {
  switch (args) {
  | [String(s)] => Keyword(":" ++ s)
  | [Keyword(s)] => Keyword(s)
  | _ => raise(Failure("wrong args to keyword"))
  };
};

let isKeyword = args => {
  switch (args) {
  | [Keyword(_)] => True
  | _ => False
  };
};

let vector = args => {
  switch (args) {
  | lst => Vector(lst)
  };
};

let isVector = args => {
  switch (args) {
  | [Vector(_)] => True
  | _ => False
  };
};

let isSequential = args => {
  switch (args) {
  | [Vector(_)] => True
  | [List(_)] => True
  | _ => False
  };
};

let hashMap = args => {
  let m =
    args
    |> Util.partition2
    |> List.fold_left((acc, (k, v)) => MalMap.add(k, v, acc), MalMap.empty);
  HashMap(m);
};

let isMap = args => {
  switch (args) {
  | [HashMap(_)] => True
  | _ => False
  };
};

let assoc = args => {
  switch (args) {
  | [HashMap(m), ...rst] =>
    let m =
      rst
      |> Util.partition2
      |> List.fold_left((acc, (k, v)) => MalMap.add(k, v, acc), m);
    HashMap(m);
  | _ => raise(Failure("Wrong args to assoc"))
  };
};

let dissoc = args => {
  switch (args) {
  | [HashMap(m), ...rst] =>
    let m = rst |> List.fold_left((acc, k) => MalMap.remove(k, acc), m);
    HashMap(m);
  | _ => raise(Failure("Wrong args to dissoc"))
  };
};

let get = args => {
  switch (args) {
  | [HashMap(m), k] =>
    switch (MalMap.find_opt(k, m)) {
    | Some(v) => v
    | None => Nil
    }
  | [Nil, _] => Nil
  | _ => raise(Failure("Wrong args to get"))
  };
};

let contains = args => {
  switch (args) {
  | [HashMap(m), k] =>
    switch (MalMap.find_opt(k, m)) {
    | Some(_) => True
    | None => False
    }
  | _ => raise(Failure("Wrong args to contains"))
  };
};

let extractBindingsHelper = (f, args) => {
  switch (args) {
  | [HashMap(m)] =>
    let ret = m |> MalMap.bindings |> List.map(f);
    List(ret);
  | _ => raise(Failure("Wrong args to keys"))
  };
};

let isString = args => {
  switch (args) {
  | [String(_)] => True
  | _ => False
  };
};

let isNumber = args => {
  switch (args) {
  | [Integer(_)] => True
  | _ => False
  };
};

let isFn = args => {
  switch (args) {
  | [Fn(_, Function)] => True
  | _ => False
  };
};

let isMacro = args => {
  switch (args) {
  | [Fn(_, Macro)] => True
  | _ => False
  };
};

let notImplemented = _ => raise(Failure("Not implemented"));

let readline = args => {
  switch (args) {
  | [String(s)] =>
    print_string(s);
    String(read_line());
  | _ => raise(Failure("wrong args to readline"))
  };
};

let timeMs = args => {
  switch (args) {
  | [] => Integer((Unix.time() |> int_of_float) * 1000)
  | _ => raise(Failure("wrong args to time-ms"))
  };
};

let stringToSeqHelper = str => {
  str
  |> String.to_seq
  |> Seq.fold_left(
       (acc, next) => acc @ [String(String.make(1, next))],
       [],
     );
};

let seq = args => {
  switch (args) {
  | [List([])]
  | [Vector([])]
  | [String("")]
  | [Nil] => Nil
  | [Vector(lst)]
  | [List(lst)] => List(lst)
  | [String(s)] => List(stringToSeqHelper(s))
  | _ => raise(Failure("wrong args to seq"))
  };
};

let conj = args => {
  switch (args) {
  | [Vector(v), ...args] => Vector(v @ args)
  | [List(lst), ...args] => List(args @ lst)
  | _ => raise(Failure("wrong args to conj"))
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
  ("list?", makeFn(isList)),
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
  ("symbol", makeFn(symbol)),
  ("symbol?", makeFn(isSymbol)),
  ("keyword", makeFn(keyword)),
  ("keyword?", makeFn(isKeyword)),
  ("vector", makeFn(vector)),
  ("vector?", makeFn(isVector)),
  ("sequential?", makeFn(isSequential)),
  ("hash-map", makeFn(hashMap)),
  ("map?", makeFn(isMap)),
  ("assoc", makeFn(assoc)),
  ("dissoc", makeFn(dissoc)),
  ("get", makeFn(get)),
  ("contains?", makeFn(contains)),
  ("vals", makeFn(x => x |> extractBindingsHelper(snd))),
  ("keys", makeFn(x => x |> extractBindingsHelper(fst))),
  ("string?", makeFn(isString)),
  ("number?", makeFn(isNumber)),
  ("fn?", makeFn(isFn)),
  ("macro?", makeFn(isMacro)),
  ("time-ms", makeFn(timeMs)),
  ("meta", makeFn(notImplemented)),
  ("with-meta", makeFn(notImplemented)),
  ("seq", makeFn(seq)),
  ("conj", makeFn(conj)),
  ("*host-language*", String("reasonml")),
  ("readline", makeFn(readline)),
];
