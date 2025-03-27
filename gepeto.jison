/* Gramática léxica. */
%lex
%%
/* Tipos de Dados */
"char" {console.log('Token CHAR'); return 'CHAR';}
"double" {console.log('Token DOUBLE'); return 'DOUBLE';}
"float" {console.log('Token FLOAT'); return 'FLOAT';}
"int" {console.log('Token INT'); return 'INT';}
"long" {console.log('Token LONG'); return 'LONG';}
"short" {console.log('Token SHORT'); return 'SHORT';}
"signed" {console.log('Token SIGNED'); return 'SIGNED';}
"unsigned" {console.log('Token UNSIGNED'); return 'UNSIGNED';}
"void" {console.log('Token VOID'); return 'VOID';}

/* Controle de Fluxo */
"if" {console.log('Token IF'); return 'IF';}
"else" {console.log('Token ELSE'); return 'ELSE';}
"switch" {console.log('Token SWITCH'); return 'SWITCH';}
"case" {console.log('Token CASE'); return 'CASE';}
"default" {console.log('Token DEFAULT'); return 'DEFAULT';}
"while" {console.log('Token WHILE'); return 'WHILE';}
"do" {console.log('Token DO'); return 'DO';}
"for" {console.log('Token FOR'); return 'FOR';}
"break" {console.log('Token BREAK'); return 'BREAK';}
"continue" {console.log('Token CONTINUE'); return 'CONTINUE';}
"goto" {console.log('Token GOTO'); return 'GOTO';}

/* Modificadores de Armazenamento */
"auto" {console.log('Token AUTO'); return 'AUTO';}
"extern" {console.log('Token EXTERN'); return 'EXTERN';}
"register" {console.log('Token REGISTER'); return 'REGISTER';}
"static" {console.log('Token STATIC'); return 'STATIC';}
"typedef" {console.log('Token TYPEDEF'); return 'TYPEDEF';}

/* Operadores e Manipulação de Memória */
"sizeof" {console.log('Token SIZEOF'); return 'SIZEOF';}

/* Manipulação de Estrutura de Dados */
"struct" {console.log('Token STRUCT'); return 'STRUCT';}
"union" {console.log('Token UNION'); return 'UNION';}
"enum" {console.log('Token ENUM'); return 'ENUM';}

/* Retorno de Função */
"return" {console.log('Token RETURN'); return 'RETURN';}

/* Modificadores de Variável */
"const" {console.log('Token CONST'); return 'CONST';}
"volatile" {console.log('Token VOLATILE'); return 'VOLATILE';}

/* Diretivas de Pré-processador */
"#" {console.log('Token #'); return '#';}
"define" {console.log('Token DEFINE'); return 'DEFINE';}
"include" {console.log('Token INCLUDE'); return 'INCLUDE';}

/* Identificadores e Variáveis */
[a-zA-Z_][a-zA-Z0-9_]* {console.log('Token IDENTIFICADOR'); return 'IDENTIFICADOR';}

/* Números (Inteiros e Decimais) */
[0-9]+ {console.log('Token INTEIRO'); return 'INT_LIT';}
[0-9]+\.[0-9]+ {console.log('Token DECIMAL'); return 'F_LIT';}

/* Operadores Aritméticos */
"=" {console.log('Token ATRIBUICAO'); return '=';}
"+" {console.log('Token SOMA'); return '+';}
"-" {console.log('Token SUBTRACAO'); return '-';}
"*" {console.log('Token MULTIPLICACAO'); return '*';}
"/" {console.log('Token DIVISAO'); return '/';}
"%" {console.log('Token MODULO'); return '%';}

/* Operadores Relacionais */
"==" {console.log('Token IGUAL'); return 'EQ';}
"!=" {console.log('Token DIFERENTE'); return 'NE';}
"<" {console.log('Token MENOR'); return '<';}
">" {console.log('Token MAIOR'); return '>';}

/* Operadores Lógicos */
"&&" {console.log('Token AND'); return 'AND';}
"||" {console.log('Token OR'); return 'OR';}
"!" {console.log('Token NOT'); return 'NOT';}

/* Operadores de Incremento e Decremento */
"++" {console.log('Token INCREMENTO'); return 'INCREMENTO';}
"--" {console.log('Token DECREMENTO'); return 'DECREMENTO';}

/* Operadores Bit a Bit */
"&" {console.log('Token BIT_AND'); return '&';}
"|" {console.log('Token BIT_OR'); return '|';}
"^" {console.log('Token BIT_XOR'); return '^';}
"~" {console.log('Token BIT_NOT'); return '~';}
"<<" {console.log('Token SHIFT_ESQUERDA'); return 'SHIFT_ESQUERDA';}
">>" {console.log('Token SHIFT_DIREITA'); return 'SHIFT_DIREITA';}

/* Delimitadores */
"(" {console.log('Token ABRE_PARENTESE'); return '(';}
")" {console.log('Token FECHA_PARENTESE'); return ')';}
"{" {console.log('Token ABRE_CHAVE'); return '{';}
"}" {console.log('Token FECHA_CHAVE'); return '}';}
"[" {console.log('Token ABRE_COLCHETE'); return '[';}
"]" {console.log('Token FECHA_COLCHETE'); return ']';}
";" {console.log('Token PONTO_E_VIRGULA'); return ';';}
"," {console.log('Token VIRGULA'); return ',';}
"." {console.log('Token PONTO'); return '.';}

/* Strings e Caracteres */
\".*\" {console.log('Token STRING'); return 'STRING';}
\'.\' {console.log('Token CARACTERE'); return 'CARACTERE';}

/* Comentários */
"//".* {console.log('Comentário de linha');}
"/*"[^*]*"*/" {console.log('Comentário de bloco');}
[ \t\n\r]+ { /* Ignorar espaços em branco */ }

. {console.log('Erro léxico: caractere [', yytext, '] não reconhecido.');}
<<EOF>> {console.log('Token EOF'); return 'EOF';}
/lex

/* Associações de operadores e precedência */
%left '<' '>' '=' EQ NE
%left '+' '-'
%left '*' '/'
%left INT_LIT
%left F_LIT
%left CHAR
%left UMINUS
%left DEFINE

/* >>> Start do Parser */
%start expressions

%%
expressions
    : e EOF
        { typeof console !== 'undefined' ? console.log($1) : print($1); return $1; }
    ;

/* Atribuições e alocações */
e
    : e '=' e
        { console.log('Atribuição: ', $1, '=', $3); $$ = $1 = $3; }
    | "malloc" '(' e ')'
        { console.log('Alocação de memória'); $$ = "malloc(" + $3 + ")"; }
    | "free" '(' e ')'
        { console.log('Desalocação de memória'); $$ = "free(" + $3 + ")"; }
    ;

/* Estruturas condicionais */
if_stmt
    : IF '(' e ')' statement ELSE statement
        { console.log('Estrutura IF-ELSE'); $$ = $3 ? $5 : $7; }
    | IF '(' e ')' statement
        { console.log('Estrutura IF'); $$ = $3 ? $5 : ""; }
    ;

e
    : e '==' e
        { console.log('Comparação de igualdade'); $$ = $1 == $3; }
    | e '!=' e
        { console.log('Comparação de desigualdade'); $$ = $1 != $3; }
    ;

/* Estruturas de repetição */
while_stmt
    : WHILE '(' e ')' statement
        { console.log('Estrutura WHILE'); $$ = $3 ? $5 : ""; }
    ;

do_while_stmt
    : DO statement WHILE '(' e ')'
        { console.log('Estrutura DO-WHILE'); $$ = $3 ? $5 : ""; }
    ;

for_stmt
    : FOR '(' e ';' e ';' e ')' statement
        { console.log('Estrutura FOR'); $$ = $3 ? $5 : ""; }
    ;

/* Operações de comparação */
comparison
    : e '<' e
        { console.log('Operação de comparação menor'); $$ = $1 < $3; }
    | e '>' e
        { console.log('Operação de comparação maior'); $$ = $1 > $3; }
    | e '<=' e
        { console.log('Operação de comparação menor ou igual'); $$ = $1 <= $3; }
    | e '>=' e
        { console.log('Operação de comparação maior ou igual'); $$ = $1 >= $3; }
    ;

    /* Diretivas de pré-processador */
preprocessor_stmt
    : '#' INCLUDE '<' IDENTIFICADOR '>'
        { console.log('Diretiva INCLUDE: ', $3); $$ = $3; }
    | '#' INCLUDE STRING
        { console.log('Diretiva INCLUDE: ', $3); $$ = $3; }
    | '#' DEFINE IDENTIFICADOR e
        { console.log('Diretiva DEFINE: ', $3, '=', $4); $$ = $4; }
    ;

include_stmt
    : '#' INCLUDE '<' IDENTIFICADOR '>'
        { console.log('Diretiva INCLUDE: ', $3); $$ = $3; }
    | '#' INCLUDE STRING
        { console.log('Diretiva INCLUDE: ', $3); $$ = $3; }
    ;


include_stmt
    : '#' INCLUDE STRING
        { console.log('Diretiva INCLUDE: ', $3); $$ = $3; }
    ;
