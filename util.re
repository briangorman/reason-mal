let take = (toTake, lst) => {
  let rec acc = (remainingToTake, taken, remaining) =>
    if (remainingToTake == 0) {
      taken;
    } else {
      switch (remaining) {
      | [next, ...rst] =>
        acc(remainingToTake - 1, List.append(taken, [next]), rst)
      | [] => raise(Failure("Invalid take"))
      };
    };
  acc(toTake, [], lst);
};

let drop = (toDrop, lst) => {
  let rec acc = (remainingToDrop, remaining) =>
    if (remainingToDrop == 0) {
      remaining;
    } else {
      switch (remaining) {
      | [_next, ...rst] => acc(remainingToDrop - 1, rst)
      | [] => raise(Failure("Invalid drop"))
      };
    };
  acc(toDrop, lst);
};

let partition2 = lst => {
  let rec acc = (l, rem) =>
    switch (rem) {
    | [fst, snd, ...rest] => acc(List.append(l, [(fst, snd)]), rest)
    | [_fst, ..._rest] => raise(Failure("wrong"))
    | [] => l
    };
  acc([], lst);
};

/* let partition = (n, lst) => { */
/*   let acc = (n, accumulated, remaining,  ) */
/* } */
