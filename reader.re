open Types;

let malRegex =
  Pcre.regexp(
    "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\\s\\[\\]{}()'\"`@,;]+)",
  );

let isStringLiteral = token =>
  if (String.length(token) == 0) {
    false;
  } else {
    token.[String.length(token) - 1] == '\n';
  };

let read_atom = token => {
  switch (token) {
  | "nil" => Nil
  | "true" => True
  | "false" => False
  | t =>
    switch (t.[0]) {
    | ':' => Keyword(t)
    | '"' =>
      isStringLiteral(t)
        ? String(Scanf.unescaped(t)) : raise(Failure("Invalid string"))

    | _ =>
      switch (int_of_string_opt(t)) {
      | Some(i) => Integer(i)
      | None => Symbol(token)
      }
    }
  };
};

let rec read_form = readerObj =>
  switch (readerObj#peek()) {
  | "(" => read_list(readerObj)
  | "[" => read_vector(readerObj)
  | "{" => read_hashmap(readerObj)
  | atom => read_atom(atom)
  }
and read_list = readerObj => {
  let rec accumulator = lst => {
    switch (readerObj#peek()) {
    | ")" => List(lst)
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
    | "]" => Vector(lst)
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
               | Symbol(s) => StringMap.add(s, v, acc)
               | Keyword(s) => StringMap.add(s, v, acc)
               | _ =>
                 raise(Failure("Only keys and symbols can be added to maps"))
               },
             StringMap.empty,
           );
      HashMap(hm);
    | _form =>
      let lst = List.append(lst, [read_form(readerObj)]);
      readerObj#next() |> ignore;
      accumulator(lst);
    };
  };
  assert(readerObj#next() == "{");
  accumulator([]);
};

let tokenize = str =>
  str
  |> Pcre.exec_all(~rex=malRegex)
  |> Array.map(Pcre.get_substrings(~full_match=false))
  |> Array.fold_left((acc, next) => Array.append(acc, next), [||]);

let read_str = str => {
  let tokens = tokenize(str);
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
