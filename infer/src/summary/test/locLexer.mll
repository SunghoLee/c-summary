(*
 * Copyright (c) 2020, SW@ Laboratory at CNU.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{
  open Lexing
  open LocParser

  exception LexError of string
  (*type token = INDEX of int | FIELD of string | AT | LPAREN | RPAREN | PTR | CONST of string | DYN of string | RET *)
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| "ret" { RET }
| "ex_" ['#''a'-'z''A'-'Z''0'-'9''_''$']+ as lxm { let name = String.sub lxm 3 ((String.length lxm) - 3) in EX name}
| "im_" _+ as lxm { let name = String.sub lxm 3 ((String.length lxm) - 3) in IM name}
| "gb_" ['#''a'-'z''A'-'Z''0'-'9''_''$''.']+ as lxm { let name = String.sub lxm 3 ((String.length lxm) - 3) in GB name}
| "fn_" ['a'-'z''A'-'Z''0'-'9''_']+ as lxm { let name = String.sub lxm 3 ((String.length lxm) - 3) in FN name}
| '\''[^'\'']+'\'' as lxm { let name = Str.global_replace (Str.regexp_string "'") "" lxm in CONST_STRING name }
| ['0'-'9']+ as lxm { CONST_INT (int_of_string lxm) }
| '@' {AT}
| '(' {LPAREN}
| ')' {RPAREN}
| '*' {PTR}
| eof {EOL}
| _ as c { raise (LexError (Format.asprintf "Invalid token: %c" c)) }
