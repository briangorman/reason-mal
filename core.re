open Types;

let numFun = (f, args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => Integer(f(a, b))
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let prn = args => {
  switch (args) {
  | [elm] =>
    print_endline(Printer.pr_str(elm));
    Nil;
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let listFn = args => {
  switch (args) {
  | toBeList => List(toBeList)
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
  | [List(lst)] => Integer(List.length(lst))
    | _ => raise(Failure("count only works on lists or nil"))
  };
};

type computationEnded =
  | EqualityComplete(malType)
  | InProgress(list(malType));


// Very Nice dual with apply
let rec equal = args => {
  switch (args) {
  | [List(lst), List(lst2)] =>
    switch (equal_list(lst, lst2)) {
    | EqualityComplete(complete) => complete
    | InProgress(lst) => equal(lst)
    }
  | [Integer(a), Integer(b)] => a == b ? True : False
  | [Symbol(s1), Symbol(s2)] => 0 == String.compare(s1, s2) ? True : False
  | [True, True] => True
  | [False , False] => True
  | [Nil, Nil] => True
  | _ => False
  };
}
and equal_list = (lst1, lst2) => switch (lst1, lst2) {
  | ([], []) => EqualityComplete(True)
  | ([_fst, ..._rst], []) => EqualityComplete(False)
  | ([], [_fst, ..._rst]) => EqualityComplete(False)
  | ([_fst1, ...rst1], [_fst2, ...rst2]) => InProgress([List(rst1), List(rst2)])

}

let intLessThan = (args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => a < b ? True : False
  | _ => raise(Failure("Wrong args or function type"))
  };
};

let intLessThanEqualTo = (args) => {
  switch (args) {
  | [Integer(a), Integer(b)] => a <= b ? True : False
  | _ => raise(Failure("Wrong args or function type"))
  };
};


let mal_complement  = (tf) => {
  switch (tf) {
  | True => False
  | False => True
    | _ => raise(Failure("bad"))
  };
};

let ns = [
  ("+", Fn(numFun((+)))),
  ("-", Fn(numFun((-)))),
  ("/", Fn(numFun((/)))),
  ("*", Fn(numFun(( * )))),
  ("prn", Fn(prn)),
  ("list", Fn(listFn)),
  ("list?", Fn(listQuestion)),
  ("empty?", Fn(listEmpty)),
  ("count", Fn(count)),
  ("=", Fn(equal)),
  ("<", Fn(intLessThan)),
  ("<=", Fn(intLessThanEqualTo)),
  (">", Fn(x => x |> intLessThanEqualTo |> mal_complement)),
  (">=", Fn(x => x |> intLessThan |> mal_complement)),
];
