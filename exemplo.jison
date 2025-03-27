%lex

%%

\s+                                 {/* ignorar */}
"int"                               {return 'INT';}
"double"                            {return 'DOUBLE';}
"float"                             {return 'FLOAT';}
"char"                              {return 'CHAR';}
"*"                                 {return '*';}
"+"                                 {return '+';}
"-"                                 {return '-';}



[a-zA-Z][a-zA-Z0-9_]*               {return 'IDF';}
[0-9]*\.[0-9]+([eE][+-][0-9]+)?     {return 'F_LIT';}
[0-9]+                              {return 'INT_LIT';}
"'"[a-zA-Z0-9_]"'"                  {return 'CHAR_LIT';}
.                                   {console.log('Erro léxico: caractere [', yytext, '] não reconhecido.');}
<<EOF>>                             {console.log('Token EOF\n'); return 'EOF';}

/lex

%start corpo

%% /* Gramática */

corpo
    : statements EOF
    ; 


statement
    : exp_stmt
    | BREAK ';' 
    | CONTINUE ';' 
    | if_stmt
    | loop_stmt
    | switch_stmt 
    | statement_composto 
    ;



/* Gramática do IF */
if_stmt
    : IF '(' expressao_condicional ')' '{' statement '}' ELSE  statement
      {console.log('IF ELSE')}
    | IF '(' expressao_condicional ')' '{' statement '}' 
      {console.log('IF Statement')}
    | IF '(' conditional_expression ')' '{' statements '}' ELSE '{' statements '}'
      {console.log('IF ELSE')}
    | IF '(' conditional_expression ')' statement
      {console.log('IF')}
    ;



/* Gramática do WHILE e do FOR */
loop_stmt
    : WHILE '(' expressao_condicional ')' '{' statement '}'
      {console.log('WHILE LOOP')}
    | FOR '(' exp_stmt  exp_stmt  expressao_atribuicao ')' statement
      {console.log('FOR LOOP')}
    | DO '{' statement '}' WHILE '(' expressao_condicional ')' ';'
      {console.log('DO WHILE LOOP')}     
    ;



/* Declaração de variável com ou sem inicialização */
declaracao_variavel
	: tipo_var IDF
	| tipo_var IDF '=' expressao_aritmetica
	;



expressao_condicional
    : IDF operador_relacional IDF
    | IDF operador_relacional valor_lit
    ;


operador_relacional
    : LE | GE  | EQ  | NE | '>' | '<'
    {$$ = $1}
    ;

value_lit
    : F_LIT
    | INT_LIT
    | CHAR_LIT
    ;

return
    : RETURN value_lit
    ;
