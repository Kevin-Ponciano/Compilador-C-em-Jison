/*Gramática léxica.*/
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
[0-9]+ {console.log('Token INTEIRO'); return 'INTEIRO';}
[0-9]+\.[0-9]+ {console.log('Token DECIMAL'); return 'DECIMAL';}

/* Operadores Aritméticos */
"=" {console.log('Token ATRIBUICAO'); return '=';}
"+" {console.log('Token SOMA'); return '+';}
"-" {console.log('Token SUBTRACAO'); return '-';}
"*" {console.log('Token MULTIPLICACAO'); return '*';}
"/" {console.log('Token DIVISAO'); return '/';}
"%" {console.log('Token MODULO'); return '%';}

/* Operadores Relacionais */
"==" {console.log('Token IGUAL'); return '==';}
"!=" {console.log('Token DIFERENTE'); return '!=';}
"<" {console.log('Token MENOR'); return '<';}
">" {console.log('Token MAIOR'); return '>';}
"<=" {console.log('Token MENORIGUAL'); return '<=';}
">=" {console.log('Token MAIORIGUAL'); return '>=';}

/* Operadores Lógicos */
"&&" {console.log('Token AND'); return '&&';}
"||" {console.log('Token OR'); return '||';}
"!" {console.log('Token NOT'); return '!';}

/* Operadores de Incremento e Decremento */
"++" {console.log('Token INCREMENTO'); return '++';}
"--" {console.log('Token DECREMENTO'); return '--';}

/* Operadores Bit a Bit */
"&" {console.log('Token BIT_AND'); return '&';}
"|" {console.log('Token BIT_OR'); return '|';}
"^" {console.log('Token BIT_XOR'); return '^';}
"~" {console.log('Token BIT_NOT'); return '~';}
"<<" {console.log('Token SHIFT_ESQUERDA'); return '<<';}
">>" {console.log('Token SHIFT_DIREITA'); return '>>';}

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
/* associações de operadores e precedência */
%left '<' '>' '=' LE GE EQ NE
%left '+' '-'
%left '*' '/'
%left INT_LIT
%left F_LIT
%left CHAR
%left UMINUS
%left DEFINE
/* >>> Start do Parser*/
%start expressions
/* >>> Gramática BNF <<<*/
// %{
// const symbolTable = {};
// // function handleCondition(condition) {}
// function handleWhile(condition, stmts) {
// //while (condition) {
// console.log(condition.type)
// for (const stmt of stmts) {
// if (stmt.type === 'return') {
// console.log("Loggei na ordem o valor -> " + stmt.value)
// // Handle assignment statements
// // For example, handle IDENTIFIER = NUMBER
// }
// }
// // }
// }
// %}
%%
expressions
: e EOF
{ typeof console !== 'undefined' ? console.log($1) : print($1);
return $1; }
;
e
: e '+' e
{$$ = $1+$3;}
| e '-' e
{$$ = $1-$3;}
| e '*' e
{$$ = $1*$3;}
| e '/' e
{$$ = $1/$3;}
| 'INT_LIT'
{$$ = parseInt(yytext);}
| 'F_LIT'
{$$ = parseFloat(yytext);}
| e ','
| e ';'
{$$ = $1;}
| e ':'
| e '.'
| 'QUOTE' e 'QUOTE'
{$$ = $2;}
| 'DQUOTE' e 'DQUOTE'
{$$ = $2;}
| '(' e ')'
{$$ = $2}
| '[' e ']'
{$$ = $2}
| '{' e '}'
{$$ = $2;}
| e '<' e
{$$ = $1<$3;}
| e '>' e
{$$ = $1>$3;}
| e '=' e
{$$ = $1=$3;}
| e 'LE' e
{$$ = $1<=$3;}
| e 'GE' e
{$$ = $1>=$3;}
| e 'EQ' e
{$$ = $1==$3;}
| e 'NE' e
{$$ = $1!=$3;}
| e 'AND' e
{$$ = $1&&$3;}
| e 'OR' e
{$$ = $1||$3;}
| 'NOT' e
{$$ = !$2;}
| 'IF'
| 'SWITCH'
| 'CASE'
| 'BREAK'
| 'DEFAULT'
| 'ELSE'
| '#'
| 'DEFINE'
| 'FOR'
// | 'WHILE'
// | 'VAR'
// | 'DO WHILE'
// | 'IDF'
| corpo
;
corpo
: statements
;
statements
: statements statement
{ $$ = (function stmts (stmts, stmt) {
const stmtsParsed = Array.isArray(stmts) ? [...stmts] : [stmts];
return [...stmtsParsed, stmt]
})($1, $2)}
// { $$ = $1 }
| statement
{$$ = $1}
;
statement
: expression_statement ';' {console.log('Expression Statement')}
| CASE {console.log('Case Statement');}
| BREAK ';' {console.log('Break Statement');}
| CONTINUE ';'{console.log('Continue Statement')}
| return ';'
| if_stmt
| switch_stmt {console.log('SWITCH Statement')}
| repeat_stmt
| for_stmt
| while_stmt
| dowhile_stmt
| assignment
| define_stmt
| IDF '-' '-' ';'
| IDF '+' '+' ';'
| IDF '+' '+'
;
conditional_expression
: INT_LIT|IDF '>' INT_LIT
{ $$ = $1 > $3 ? true : false }
| INT_LIT 'GE' INT_LIT
{ $$ = $1 >= $3 ? true : false }
| INT_LIT|IDF '<' INT_LIT
{ $$ = $1 < $3 ? true : false }
| INT_LIT 'LE' INT_LIT
{ $$ = $1 <= $3 ? true : false }
| INT_LIT 'EQ' INT_LIT
{ $$ = $1 == $3 ? true : false }
| INT_LIT 'NE' INT_LIT
{ $$ = $1 != $3 ? true : false }
| '(' e ')' && '(' e ')'
| IDF '>' '-' INT_LIT
| IDF 'EQ' '-' INT_LIT
{ $$ = $1 == $3 ? true : false }
;
constant
: INT_LIT { $$ = $1; } // Se a constante for um valor inteiro
| F_LIT { $$ = $1; } // Se a constante for um valor de ponto flutuante
| IDF { $$ = $1; } // Se a constante for um identificador (variável)
;
if_stmt
: IF '(' conditional_expression ')' statement ELSE statement
{ $$ = $3 ? $5 : $7}
| IF '(' conditional_expression ')' statement
{ $$ = $3 ? $5 : "" }
| IF '(' conditional_expression ')' '{' statements '}' ELSE '{' statements '}'
{ $$ = $3 ? $6 : $10}
| IF '(' conditional_expression ')' '{' statements '}'
{ $$ = $3 ? $6 : "" }
;
case_list
: CASE constant ':' statement case_list
{$$ = $5}
| CASE constant ':' case_list
{$$ = $4}
| CASE constant ':' statement
{$$ = $4}
| CASE constant ':' statements
{$$ = $4}
;
switch_stmt
: SWITCH '(' e ')' '{' case_list '}'
{$$ = $6}
| SWITCH '(' e ')' '{' case_list default_case '}'
{$$ = $7}
;
default_case
: DEFAULT ':' statement
{$$ = $3}
;
INTS_LIT
: INTS_LIT ',' INT_LIT
| INT_LIT
;
assignment
: FLOAT IDF '=' F_LIT ';' { $2 = $4; $$ = $2}
| INT IDF '=' INT_LIT ';' { symbolTable[$2] = $4}
| CHAR IDF '=' CHAR ';' { $2 = $4; $$ = $2}
| IDF '=' INT_LIT ';' { symbolTable[$2] = $4}
| INT IDF '[' INT_LIT ']' '=' '{' INTS_LIT '}' ';'
| INT IDF ',' IDF? ';'
| INT IDF ';'
| IDF '+''=' IDF'['IDF']' ';'
;
for_stmt
: FOR '(' init_expr ';' conditional_expression ';' e ')' '{' statements '}'
;
init_expr
: INT_LIT
| INT IDF '=' INT_LIT { $2 = $4; $$ = $2}
| IDF '=' INT_LIT { $1 = $3; $$ = $1}
;
while_stmt
: WHILE '(' conditional_expression ')' '{' statements '}'
{ handleWhile($3, $6) }
;
dowhile_stmt
: DO '{' statements '}' WHILE '(' conditional_expression ')' ';'
;
value_lit
: F_LIT
| INT_LIT
| CHAR_LIT
;
return //aqui tinha ';' depois de value_lit e idf, mas duplica
: RETURN value_lit
{ $$ = {type: "return", value: $2 } }
// {type: "return", value: $2 }
| RETURN IDF
{ $$ = symbolTable[$2]; console.log("symbol table"); console.log(symbolTable) }
;
define_stmt
: '#' DEFINE IDF value_lit
{$$ = $4}
;
/* >>> Fim da Gramática BNF*/