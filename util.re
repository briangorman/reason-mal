let partition2 = lst => {
  let rec acc = (l, rem) =>
    switch (rem) {
    | [fst, snd, ...rest] => acc(List.append(l, [(fst, snd)]), rest)
    | [_fst, ..._rest] => raise(Failure("wrong"))
    | [] => l
    };
  acc([], lst);
};
