%lex
%%
\s+ {console.log('BRANCO'); /ignorar/}
"int" {console.log('Token INT'); return 'INT';}
"double" {console.log('Token DOUBLE'); return 'DOUBLE';}
"float" {console.log('Token FLOAT'); return 'FLOAT';}
"char" {console.log('Token CHAR'); return 'CHAR';}
"" {console.log('Token *'); return '';}
"+" {console.log('Token +'); return '+';}
"-" {console.log('Token -'); return '-';}
"/" {console.log('Token /'); return '/';}
"," {console.log('Token ,'); return ',';}
";" {console.log('Token ;'); return ';';}
":" {console.log('Token :' ); return ':';}
"." {console.log('Token .'); return '.';}
"'" {console.log('Token QUOTE'); return 'QUOTE';}
'"' {console.log('Token DQUOTE'); return 'DQUOTE';}
"(" {console.log('Token (' ); return '(';}
")" {console.log('Token )' ); return ')';}
"[" {console.log('Token ['); return '[';}
"]" {console.log('Token ]'); return ']';}
"{" {console.log('Token {'); return '{';}
"}" {console.log('Token }'); return '}';}
"<=" {console.log('Token <='); return 'LE';}
"<" {console.log('Token <'); return '<';}
">=" {console.log('Token >='); return 'GE';}
">" {console.log('Token >'); return '>';}
"==" {console.log('Token =='); return 'EQ';}
"=" {console.log('Token ='); return '=';}
"!=" {console.log('Token !='); return 'NE';}
"&&" {console.log('Token &&'); return 'AND';}
"||" {console.log('Token ||'); return 'OR';}
"!" {console.log('Token !'); return 'NOT';}
"if" {console.log('Token IF'); return 'IF';}
"switch" {console.log('Token SWITCH'); return 'SWITCH';}
"case" {console.log('Token CASE'); return 'CASE';}
"break" {console.log('Token BREAK'); return 'BREAK';}
"default" {console.log('Token DEFAULT'); return 'DEFAULT';}
"else" {console.log('Token ELSE'); return 'ELSE';}
"while" {console.log('Token WHILE'); return 'WHILE';}
"for" {console.log('Token FOR'); return 'FOR';}
"variavel" {console.log('Token VAR'); return 'VAR';}
"do" {console.log('Token DO WHILE'); return 'DO';}
"#" {console.log('Token #'); return '#';}
"define" {console.log('Token DEFINE'); return 'DEFINE';}
"return" {console.log('Token RETURN'); return 'RETURN';}
[a-zA-Z][a-zA-Z0-9_]* {console.log('Token IDF'); return 'IDF';}[0-9]*\.[0-9]+([eE][+-][0-9]+)? {console.log('Token F_LIT'); return 'F_LIT';}
[0-9]+ {console.log('Token INT_LIT'); return 'INT_LIT';}
\w {console.log('Token CHAR'); return 'CHAR';}
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
%{
const symbolTable = {};
// function handleCondition(condition) {}
function handleWhile(condition, stmts) {
//while (condition) {
console.log(condition.type)
for (const stmt of stmts) {
if (stmt.type === 'return') {
console.log("Loggei na ordem o valor -> " + stmt.value)
// Handle assignment statements
// For example, handle IDENTIFIER = NUMBER
}
}
// }
}
%}
%%
expressions
: e EOF
{ typeof console !== 'undefined' ? console.log($1) : print($1);
return $1; }
;e
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
| 'BREAK'| 'DEFAULT'
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
| INT_LIT 'GE' INT_LIT{ $$ = $1 >= $3 ? true : false }
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
: DEFAULT ':' statement{$$ = $3}
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
;define_stmt
: '#' DEFINE IDF value_lit
{$$ = $4}
;
/* >>> Fim da Gramática BNF*/