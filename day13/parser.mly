%token <int> INT
%token LBRACKET RBRACKET
%token COMMA

%start <Ast.t> expr
%%

expr:
    | INT { Num $1 }
    | LBRACKET l = separated_list(COMMA, expr) RBRACKET { Values l }
