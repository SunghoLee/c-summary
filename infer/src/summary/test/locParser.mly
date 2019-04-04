%token <int> INDEX
%token <string> FIELD
%token <string> CONST
%token <string> DYN
%token PTR RET
%token LPAREN RPAREN 
%token EOL
%type <Yloc.t> main
%start main
%%

main:
  loc EOL { $1 }
  ;
loc:
  | RET { Yloc.mk_ret () }
  | CONST { Yloc.mk_const $1 }
  | DYN { Yloc.mk_dyn $1 }
  | LPAREN loc RPAREN { $2 }
  | PTR loc { Yloc.mk_pointer $2 }
  | loc index { Yloc.mk_offset $1 $2 }
  ;
index:
  | INDEX { Yloc.mk_index_of_int $1 }
  | FIELD { Yloc.mk_index_of_string $1 }
