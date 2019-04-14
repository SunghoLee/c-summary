%token <string> EX
%token <string> IM
%token <string> GB
%token <string> FN
%token <string> CONST_STRING
%token <int> CONST_INT
%token PTR RET AT
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
  | EX { Yloc.mk_ex $1 }
  | IM { Yloc.mk_im $1 }
  | GB { Yloc.mk_gb $1 }
  | FN { Yloc.mk_fn $1 }
  | LPAREN loc RPAREN { $2 }
  | PTR loc { Yloc.mk_pointer $2 }
  | loc AT loc { Yloc.mk_offset $1 $3 }
  | CONST_STRING { Yloc.mk_const_of_string $1 }
  | CONST_INT { Yloc.mk_const_of_int $1 }
