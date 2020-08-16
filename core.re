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
    print_endline(Printer.pr_str(~print_readably=true, elm));
    Nil;
  | _ => raise(Failure("Wrong args or function type"))
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

let println = args => {
  switch (args) {
  | [] => raise(Failure("Wrong args or function type"))
  | lst =>
    lst
    |> List.map(Printer.pr_str(~print_readably=false))
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
  | [False, False] => True
  | [Nil, Nil] => True
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

let ns = [
  ("+", Fn(numFun((+)))),
  ("-", Fn(numFun((-)))),
  ("/", Fn(numFun((/)))),
  ("*", Fn(numFun(( * )))),
  ("prn", Fn(prn)),
  ("str", Fn(str)),
  ("pr-str", Fn(pr_str)),
  ("println", Fn(println)),
  ("list", Fn(listFn)),
  ("list?", Fn(listQuestion)),
  ("empty?", Fn(listEmpty)),
  ("count", Fn(count)),
  ("=", Fn(equal)),
  ("<", Fn(intLessThan)),
  ("<=", Fn(intLessThanEqualTo)),
  (">", Fn(x => x |> intLessThanEqualTo |> mal_complement)),
  (">=", Fn(x => x |> intLessThan |> mal_complement)),
  ("slurp", Fn(slurp)),
  ("read-string", Fn(read_str)),
];
