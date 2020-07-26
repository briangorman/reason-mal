/* Add a file printer.qx. This file will contain a single function pr_str which does the opposite of read_str: take a mal data structure and return a string representation of it.
   But pr_str is much simpler and is basically just a switch statement on the type of the input object:

symbol: return the string name of the symbol
number: return the number as a string
list: iterate through each element of the list calling pr_str on it, then join the results with a space separator, and surround the final result with parens
   */

open Types;

let rec pr_str = form => switch (form) {
  | Integer(i) => string_of_int(i)
  | Symbol(s) => s
  | List(lst) => ["(", (List.map(pr_str, lst) |> String.concat(" ")), ")"  ] |> String.concat("")
  | Fn(_) => "#<function>"
  | Nil => "nil"
  | False => "false"
  | True => "true"
}
