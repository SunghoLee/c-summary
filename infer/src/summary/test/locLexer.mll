{
  open Lexing
  open LocParser

  exception LexError of string
  (*type token = INDEX of int | FIELD of string | AT | LPAREN | RPAREN | PTR | CONST of string | DYN of string | RET *)
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| '@' ['0'-'9']+ as lxm { let name = String.sub lxm 1 ((String.length lxm) - 1 ) in INDEX (int_of_string name) }
| "ret" { RET }
| "loc_" ['a'-'z''A'-'Z''0'-'9''_''$']+ as lxm { let name = String.sub lxm 4 ((String.length lxm) - 4) in CONST name}
| "dyn_" _+ as lxm { let name = String.sub lxm 4 ((String.length lxm) - 4) in DYN name}
| '@' ['a'-'z''A'-'Z''_'] ['a'-'z''A'-'Z''0'-'9''_']+ as lxm { let name = String.sub lxm 1 ((String.length lxm) - 1 ) in FIELD name}
| '(' {LPAREN}
| ')' {RPAREN}
| '*' {PTR}
| eof {EOL}
| _ as c { raise (LexError (Format.asprintf "Invalid token: %c" c)) }
