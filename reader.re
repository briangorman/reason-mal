module T = Types.MalType;
module MalMap = Types.MalMap;

let malRegex =
  Pcre.regexp(
    "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}()'\"`@,;]+)",
  );

let isStringLiteral = token =>
  if (String.length(token) == 0) {
    false;
  } else {
    token.[String.length(token) - 1] == '"';
  };

let read_atom = token => {
  switch (token) {
  | "nil" => T.Nil
  | "true" => T.True
  | "false" => T.False
  | t =>
    switch (t.[0]) {
    | ':' => T.Keyword(t)
    | '"' =>
      isStringLiteral(t)
        ? T.String(
            Scanf.unescaped(String.sub(t, 1, String.length(t) - 2)),
          )
        : raise(Failure("Invalid string"))

    | _ =>
      switch (int_of_string_opt(t)) {
      | Some(i) => T.Integer(i)
      | None => T.Symbol(token)
      }
    }
  };
};

let rec read_form = readerObj => {
  switch (readerObj#peek()) {
  | "@" => handleReaderMacro("deref", readerObj)
  | "'" => handleReaderMacro("quote", readerObj)
  | "`" => handleReaderMacro("quasiquote", readerObj)
  | "~@" => handleReaderMacro("splice-unquote", readerObj)
  | "~" => handleReaderMacro("unquote", readerObj)
  | "(" => read_list(readerObj)
  | "[" => read_vector(readerObj)
  | "{" => read_hashmap(readerObj)
  | atom =>
    if (atom.[0] == ';') {
      readerObj#next() |> ignore;
      read_form(readerObj);
    } else {
      read_atom(atom);
    }
  };
}
and read_list = readerObj => {
  let rec accumulator = lst => {
    switch (readerObj#peek()) {
    | ")" => T.List(lst)
    | _form =>
      let newList = List.append(lst, [read_form(readerObj)]);
      readerObj#next() |> ignore;
      accumulator(newList);
    };
  };
  assert(readerObj#next() == "(");
  accumulator([]);
}
and read_vector = readerObj => {
  let rec accumulator = lst => {
    switch (readerObj#peek()) {
    | "]" => T.Vector(lst)
    | _form =>
      let newVector = List.append(lst, [read_form(readerObj)]);
      readerObj#next() |> ignore;
      accumulator(newVector);
    };
  };
  assert(readerObj#next() == "[");
  accumulator([]);
}
and read_hashmap = readerObj => {
  let rec accumulator = lst => {
    switch (readerObj#peek()) {
    | "}" =>
      let hm =
        lst
        |> Util.partition2
        |> List.fold_left(
             (acc, (k, v)) =>
               switch (k) {
               | T.String(_) => MalMap.add(k, v, acc)
               | T.Keyword(_) => MalMap.add(k, v, acc)
               | _ => raise(Failure("Only keys and can be added to maps"))
               },
             MalMap.empty,
           );
      T.HashMap(hm);
    | _form =>
      let lst = List.append(lst, [read_form(readerObj)]);
      readerObj#next() |> ignore;
      accumulator(lst);
    };
  };
  assert(readerObj#next() == "{");
  accumulator([]);
}
and handleReaderMacro = (sym, readerObj) => {
  readerObj#next() |> ignore;
  T.List([T.Symbol(sym), read_form(readerObj)]);
};

let tokenize = str =>
  str
  |> Pcre.exec_all(~rex=malRegex)
  |> Array.map(Pcre.get_substrings(~full_match=false))
  |> Array.fold_left((acc, next) => Array.append(acc, next), [||]);

let read_str = str => {
  let tokens = tokenize(str);

  if (Array.mem("\"", tokens)) {
    raise(Failure("Error: Unbalanced string"));
  };

  let index = ref(0);
  let reader = {
    as _;
    pub next = () => {
      let currentToken = tokens[index^];
      index := index^ + 1;
      currentToken;
    };
    pub peek = () => tokens[index^]
  };
  read_form(reader);
};
