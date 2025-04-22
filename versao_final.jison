/* =====================================================
 * compilador_c_completo.jison — Subconjunto ANSI‑C para Jison
 * =====================================================*/

/* ---------- 1. Léxico -------------------------------- */
%lex
%%
\s+                            /* ignorar espaços */
"//"[^\n]*                     /* comentário de linha */
\/\*[\s\S]*?\*\/               /* comentário multilinha */

"<"[A-Za-z0-9_\/\.]+">"        return 'HEADER';

/* palavras‑reservadas */
"&"            return '&';
"auto"\b       return 'AUTO';
"break"\b      return 'BREAK';
"case"\b       return 'CASE';
"char"\b       return 'CHAR_T';
"const"\b      return 'CONST';
"continue"\b   return 'CONTINUE';
"default"\b    return 'DEFAULT';
"do"\b         return 'DO';
"double"\b     return 'DOUBLE_T';
"else"\b       return 'ELSE';
"enum"\b       return 'ENUM';
"extern"\b     return 'EXTERN';
"float"\b      return 'FLOAT_T';
"for"\b        return 'FOR';
"goto"\b       return 'GOTO';
"if"\b         return 'IF';
"int"\b        return 'INT_T';
"long"\b       return 'LONG_T';
"register"\b   return 'REGISTER';
"return"\b     return 'RETURN';
"short"\b      return 'SHORT_T';
"signed"\b     return 'SIGNED';
"sizeof"\b     return 'SIZEOF';
"static"\b     return 'STATIC';
"struct"\b     return 'STRUCT';
"switch"\b     return 'SWITCH';
"typedef"\b    return 'TYPEDEF';
"union"\b      return 'UNION';
"unsigned"\b   return 'UNSIGNED';
"void"\b       return 'VOID';
"volatile"\b   return 'VOLATILE';
"while"\b      return 'WHILE';

/* diretivas */
"define"\b     return 'DEFINE';
"include"\b    return 'INCLUDE';

/* operadores compostos e símbolos */
"++"           return 'INC';
"--"           return 'DEC';
"=="           return 'EQ';
"!="           return 'NE';
"<="           return 'LE';
">="           return 'GE';
"&&"           return 'AND';
"||"           return 'OR';
"<<="          return 'LSHIFT_ASSIGN';
">>="          return 'RSHIFT_ASSIGN';
"<<"           return 'LSHIFT';
">>"           return 'RSHIFT';
"+="           return 'PLUS_ASSIGN';
"-="           return 'MINUS_ASSIGN';
"*="           return 'MUL_ASSIGN';
"/="           return 'DIV_ASSIGN';
"%="           return 'MOD_ASSIGN';
"&="           return 'AND_ASSIGN';
"|="           return 'OR_ASSIGN';
"^="           return 'XOR_ASSIGN';
"->"           return 'ARROW';

/* operadores e símbolos simples */
"!"            return '!';
"?"            return '?';
":"            return ':';
"["            return '[';
"]"            return ']';
"{"            return '{';
"}"            return '}';
"("            return '(';
")"            return ')';
","            return ',';
";"            return ';';
"#"            return '#';
"."            return '.';
"+"            return '+';
"-"            return '-';
"*"            return '*';
"/"            return '/';
"%"            return '%';
"="            return '=';
"<"            return '<';
">"            return '>';

/* literais */
[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?    return 'F_LIT';
[0-9]+                             return 'INT_LIT';
\'([^\\\']|\\.)\'                  return 'CHAR_LIT';
\"(\\.|[^"\\])*\"                  return 'STR_LIT';
[A-Za-z_][A-Za-z0-9_]*             return 'ID';

<<EOF>>        return 'EOF';
.    { console.error("Caractere desconhecido: " + yytext); return 'UNKNOWN'; }
/lex


/* ---------- 2. Tokens & Precedência ---------- */
%start program
%token AUTO BREAK CASE CHAR_T CONST CONTINUE DEFAULT DO DOUBLE_T ELSE ENUM EXTERN FLOAT_T FOR GOTO IF INT_T LONG_T REGISTER RETURN SHORT_T SIGNED SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE
%token DEFINE INCLUDE HEADER
%token INC DEC EQ NE LE GE AND OR LSHIFT RSHIFT LSHIFT_ASSIGN RSHIFT_ASSIGN PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN ARROW
%token INT_LIT F_LIT CHAR_LIT STR_LIT ID UNKNOWN
%token INCLUDE_DIRECTIVE

%left ','
%right '=' PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN
%right '?'
%right ':'
%left OR
%left AND
%left '|' '^' '&'
%left EQ NE
%left '<' '>' LE GE
%left LSHIFT RSHIFT
%left '+' '-'
%left '*' '/' '%'
%right UMINUS

/* ---------- 3. Gramática ---------- */
%%
program
  : external_list EOF                        { }
  ;

external_list
  : external_list external
  | external
  ;

external
  : preprocessor_directive
  | function_definition
  | declaration
  ;

preprocessor_directive
  : '#' INCLUDE HEADER
  | '#' DEFINE ID define_value_opt
  ;

define_value_opt
  : expression
  | STR_LIT
  | /* vazio */
  ;

include_dir
  : '#' INCLUDE HEADER  /* removido ';'? */
  ;

include_dir
  : INCLUDE_DIRECTIVE ';'?
  ;

declaration
  : declaration_specifiers init_declarator_list ';'
  ;

function_definition
  : declaration_specifiers declarator compound_statement
  ;


declaration_specifiers
  : declaration_specifiers type_specifier
  | type_specifier
  ;


type_specifier
  : CHAR_T
  | INT_T
  | FLOAT_T
  | DOUBLE_T
  | VOID
  | struct_specifier
  | union_specifier
  | enum_specifier
  ;


struct_specifier
  : STRUCT ID '{' struct_declaration_list '}'
  | STRUCT '{' struct_declaration_list '}'
  | STRUCT ID
  ;

struct_declaration_list
  : struct_declaration
  | struct_declaration_list struct_declaration
  ;

struct_declaration
  : declaration_specifiers struct_declarator_list ';'
  ;

struct_declarator_list
  : struct_declarator
  | struct_declarator_list ',' struct_declarator
  ;

struct_declarator
  : declarator
  ;



init_declarator_list
  : init_declarator
  | init_declarator_list ',' init_declarator
  ;

init_declarator
  : declarator
  | declarator '=' initializer
  ;

declarator
  : pointer_opt direct_declarator
  ;


pointer_opt
  : '*' pointer_opt
  | /* vazio */
  ;

direct_declarator
  : ID
  | '(' declarator ')'
  | direct_declarator '[' constant_expression_opt ']'
  | direct_declarator '(' parameter_list_opt ')'
  ;


parameter_list_opt_or_void
  : parameter_list
  | VOID
  | /* vazio */
  ;


constant_expression_opt
  : constant_expression
  | /* vazio */
  ;

constant_expression
  : conditional_expression
  ;

initializer
  : assignment_expression
  | '{' initializer_list '}'
  ;

initializer_list
  : initializer
  | initializer_list ',' initializer
  ;

parameter_list_opt
  : parameter_list
  | /* vazio */
  ;

parameter_list
  : parameter
  | parameter_list ',' parameter
  ;

parameter
  : declaration_specifiers declarator
  ;


compound_statement
  : '{' declaration_list_opt statement_list_opt '}'
  ;

declaration_list_opt
  : declaration_list
  | /* vazio */
  ;

declaration_list
  : declaration_list declaration
  | declaration
  ;

statement_list_opt
  : statement_list
  | /* vazio */
  ;

statement_list
  : statement_list statement
  | statement
  ;

statement
  : labeled_statement
  | compound_statement
  | expression_statement
  | selection_statement
  | iteration_statement
  | jump_statement
  ;

labeled_statement
  : CASE constant_expression ':' statement
  | DEFAULT ':' statement
  ;

expression_statement
  : expression_opt ';'
  ;

expression_opt
  : expression
  | /* vazio */
  ;

selection_statement
  : IF '(' expression ')' statement
  | IF '(' expression ')' statement ELSE statement
  ;

iteration_statement
  : WHILE '(' expression ')' statement
  | DO statement WHILE '(' expression ')' ';'
  | FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
  ;

jump_statement
  : GOTO ID ';'
  | CONTINUE ';'
  | BREAK ';'
  | RETURN expression_opt ';'
  ;

/* ------------- EXPRESSÕES ------------- */
expression
  : assignment_expression
  | expression ',' assignment_expression
  ;

assignment_expression
  : conditional_expression
  | unary_expression assignment_operator assignment_expression
  ;

assignment_operator
  : '=' | PLUS_ASSIGN | MINUS_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | AND_ASSIGN | OR_ASSIGN | XOR_ASSIGN | LSHIFT_ASSIGN | RSHIFT_ASSIGN
  ;

conditional_expression
  : logical_or_expression
  | logical_or_expression '?' expression ':' conditional_expression
  ;

logical_or_expression
  : logical_and_expression
  | logical_or_expression OR logical_and_expression
  ;

logical_and_expression
  : inclusive_or_expression
  | logical_and_expression AND inclusive_or_expression
  ;
inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression '|' exclusive_or_expression
  ;

exclusive_or_expression
  : and_expression
  | exclusive_or_expression '^' and_expression
  ;

and_expression
  : equality_expression
  | and_expression '&' equality_expression
  ;

equality_expression
  : relational_expression
  | equality_expression EQ relational_expression
  | equality_expression NE relational_expression
  ;

relational_expression
  : shift_expression
  | relational_expression '<' shift_expression
  | relational_expression '>' shift_expression
  | relational_expression LE shift_expression
  | relational_expression GE shift_expression
  ;

shift_expression
  : additive_expression
  | shift_expression LSHIFT additive_expression
  | shift_expression RSHIFT additive_expression
  ;

additive_expression
  : multiplicative_expression
  | additive_expression '+' multiplicative_expression
  | additive_expression '-' multiplicative_expression
  ;

multiplicative_expression
  : cast_expression
  | multiplicative_expression '*' cast_expression
  | multiplicative_expression '/' cast_expression
  | multiplicative_expression '%' cast_expression
  ;

cast_expression
  : unary_expression
  | '(' type_name ')' cast_expression
  ;

unary_expression
  : postfix_expression
  | INC unary_expression
  | DEC unary_expression
  | unary_operator cast_expression
  | SIZEOF unary_expression
  | SIZEOF '(' type_name ')'
  ;


unary_operator
  : '&'
  | '*'
  | '+'
  | '-'
  | '!'
  ;

postfix_expression
  : primary_expression
  | postfix_expression '[' expression ']'
  | postfix_expression '(' argument_expression_list_opt ')'
  | postfix_expression '.' ID
  | postfix_expression ARROW ID
  | postfix_expression INC
  | postfix_expression DEC
  ;

argument_expression_list_opt
  : argument_expression_list
  | /* vazio */
  ;

argument_expression_list
  : assignment_expression
  | argument_expression_list ',' assignment_expression
  ;

primary_expression
  : ID
  | INT_LIT
  | F_LIT
  | CHAR_LIT
  | STR_LIT
  | '(' expression ')'
  ;

type_name
  : type_specifier
  | type_specifier pointer
  ;

pointer
  : '*' 
  | '*' pointer
  ;

union_specifier
  : UNION ID '{' struct_declaration_list '}'
  | UNION '{' struct_declaration_list '}'
  | UNION ID
  ;

enum_specifier
  : ENUM ID '{' enumerator_list '}'
  | ENUM '{' enumerator_list '}'
  | ENUM ID
  ;

enumerator_list
  : enumerator
  | enumerator_list ',' enumerator
  ;

enumerator
  : ID
  | ID '=' constant_expression
  ;

%%
