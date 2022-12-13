{open Parser}

rule read =
    parse
    | ['0'-'9']+ {INT (int_of_string (Lexing.lexeme lexbuf))}
    | '[' {LBRACKET}
    | ']' {RBRACKET}
    | ',' {COMMA}
