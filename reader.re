open Types;

let malRegex =
  Pcre.regexp(
    "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\\s\\[\\]{}()'\"`@,;]+)",
  );

let read_atom = token => {
  switch (int_of_string_opt(token)) {
  | Some(i) => MalAtom(MalInteger(i))
  | None => MalAtom(MalSymbol(token));
  };
};

let rec read_form = readerObj =>
  switch (readerObj#peek()) {
  | "(" => read_list(readerObj)
  | atom => read_atom(atom)
  }
and read_list = readerObj => {
  let rec accumulator = (lst) => {
    let x = readerObj#peek();
    print_endline(x);
    switch (readerObj#peek()) {
    | ")" => MalList(lst)
    | _form =>
      let newAst = List.append(lst, [read_form(readerObj)]);
      readerObj#next() |> ignore;
      accumulator(newAst);
    };
  };
  assert(readerObj#next() == "(")
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
    pub next = () => {
      let currentToken = tokens[index^];
      index := index^ + 1;
      currentToken;
    };
    pub peek = () => tokens[index^]
  };
  read_form(reader);
};
