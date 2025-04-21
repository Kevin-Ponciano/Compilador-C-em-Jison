/* =====================================================
 *  compilador_c_ok.jison – Subconjunto C compatível Jison
 *  (include <header>, casts, main() {}, malloc, etc.)
 * =====================================================*/

/* ---------- 1. LÉXICO ---------- */
%lex
%%
"char"          return 'CHAR';
"double"        return 'DOUBLE';
"float"         return 'FLOAT';
"int"           return 'INT';
"long"          return 'LONG';
"short"         return 'SHORT';
"signed"        return 'SIGNED';
"unsigned"      return 'UNSIGNED';
"void"          return 'VOID';

"if"            return 'IF';
"else"          return 'ELSE';
"switch"        return 'SWITCH';
"case"          return 'CASE';
"default"       return 'DEFAULT';
"while"         return 'WHILE';
"do"            return 'DO';
"for"           return 'FOR';
"break"         return 'BREAK';
"continue"      return 'CONTINUE';
"goto"          return 'GOTO';

"auto"          return 'AUTO';
"extern"        return 'EXTERN';
"register"      return 'REGISTER';
"static"        return 'STATIC';
"typedef"       return 'TYPEDEF';
"const"         return 'CONST';
"volatile"      return 'VOLATILE';

"struct"        return 'STRUCT';
"union"         return 'UNION';
"enum"          return 'ENUM';

"return"        return 'RETURN';
"sizeof"        return 'SIZEOF';

"#"             return '#';
"define"        return 'DEFINE';
"include"       return 'INCLUDE';

"<"[a-zA-Z0-9_./]+">"         return 'ANGLE_HEADER';
[a-zA-Z_][a-zA-Z0-9_]*        return 'IDENT';
[0-9]+\.[0-9]+                return 'F_LIT';
[0-9]+                        return 'INT_LIT';

"<=" return 'LE';
">=" return 'GE';
"==" return 'EQ';
"!=" return 'NE';
"&&" return 'AND';
"||" return 'OR';
"<<" return 'SHL';
">>" return 'SHR';
"++" return 'INC';
"--" return 'DEC';

[=+\-*\/%<>!&\|\^~]           return yytext;
[\(\)\{\}\[\];,.:]            return yytext;

\"([^"\\]|\\.)*\"            return 'STRING';
\'.\'                        return 'CHAR_LIT';

"//".*                       /* skip */;
"/*"[^*]*"*/"                /* skip */;
[ \t\r\n]+                   /* skip */;

.                            { throw new Error('Caractere inesperado: '+yytext); }
<<EOF>>                      return 'EOF';
/lex

/* ---------- 2. PRECEDÊNCIA ---------- */
%start program
%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%left OR
%left AND
%left EQ NE
%left '<' '>' LE GE
%left '+' '-'
%left '*' '/' '%'
%right '!'
%right '='
%right UMINUS

%%

/* ---------- 3. GRAMÁTICA ---------- */

/* Unidade de tradução */
program
    : translation_unit EOF                     { return $1; }
    ;

translation_unit
    : /* vazio */                              { $$ = []; }
    | translation_unit external_decl           { $$ = $1.concat($2); }
    ;

external_decl
    : function_def
    | declaration
    | preprocessor_stmt
    ;

/* Pré‑processador */
preprocessor_stmt
    : '#' INCLUDE ANGLE_HEADER
    | '#' INCLUDE STRING
    | '#' DEFINE IDENT expression
    ;

/* Declarações */
declaration
    : decl_spec init_decl_list_opt ';'
    | struct_union_enum_def
    ;

init_decl_list_opt
    : /* vazio */
    | init_decl_list
    ;

init_decl_list
    : init_declarator
    | init_decl_list ',' init_declarator
    ;

init_declarator
    : declarator
    | declarator '=' initializer
    ;

initializer
    : assignment_expression
    | '{' initializer_list '}'
    ;

initializer_list
    : initializer
    | initializer_list ',' initializer
    ;

/* Especificadores */
decl_spec
    : type_specifier
    | decl_spec type_specifier
    | decl_spec storage_class_spec
    | decl_spec type_qualifier
    ;

type_specifier
    : VOID | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE | SIGNED | UNSIGNED
    | STRUCT IDENT
    | UNION IDENT
    | ENUM IDENT
    | IDENT /* alias via typedef */
    ;

storage_class_spec
    : AUTO | REGISTER | STATIC | EXTERN | TYPEDEF
    ;

type_qualifier
    : CONST | VOLATILE
    ;

/* Declarator */
declarator
    : pointer_opt direct_declarator
    ;

pointer_opt
    : /* vazio */
    | '*' pointer_opt
    ;

/* direct_declarator & param_list_opt */
direct_declarator
    : IDENT
    | IDENT '(' ')'
    | IDENT '(' VOID ')'
    | direct_declarator '(' ')'
    | direct_declarator '(' param_list_opt ')'
    | direct_declarator '[' constant_expr_opt ']'
    ;

param_list_opt
    : /* vazio */
    | parameter_list
    ;

parameter_list
    : parameter_decl
    | parameter_list ',' parameter_decl
    ;

parameter_decl
    : decl_spec declarator_opt
    ;

declarator_opt
    : /* vazio */
    | declarator
    ;

constant_expr_opt
    : /* vazio */
    | assignment_expression
    ;

/* struct / union / enum */
struct_union_enum_def
    : STRUCT IDENT '{' struct_decl_list '}' ';'
    | UNION IDENT '{' struct_decl_list '}' ';'
    | ENUM IDENT '{' enum_list '}' ';'
    ;

struct_decl_list
    : declaration
    | struct_decl_list declaration
    ;

enum_list
    : IDENT
    | enum_list ',' IDENT
    ;

/* Função */
function_def
    : decl_spec declarator compound_stmt
    ;

/* Sentenças */
compound_stmt
    : '{' stmt_list_opt '}'                 { $$ = $2; }
    ;

stmt_list_opt
    : /* vazio */                           { $$ = []; }
    | stmt_list
    ;

stmt_list
    : statement
    | stmt_list statement
    ;

statement
    : expression_stmt
    | declaration
    | compound_stmt
    | selection_stmt
    | iteration_stmt
    | jump_stmt
    | ';'
    ;

expression_stmt
    : expression_opt ';'
    ;

expression_opt
    : /* vazio */
    | expression
    ;

/* Controle de fluxo */
selection_stmt
    : IF '(' expression ')' statement                     %prec LOWER_THAN_ELSE
    | IF '(' expression ')' statement ELSE statement
    | SWITCH '(' expression ')' compound_stmt
    ;

iteration_stmt
    : WHILE '(' expression ')' statement
    | DO statement WHILE '(' expression ')' ';'
    | FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
    ;

jump_stmt
    : BREAK ';'
    | CONTINUE ';'
    | RETURN expression_opt ';'
    | GOTO IDENT ';'
    ;

/* EXPRESSÕES */
expression
    : assignment_expression
    | expression ',' assignment_expression
    ;

assignment_expression
    : logical_or_expr
    | unary_expr '=' assignment_expression
    ;

logical_or_expr
    : logical_and_expr
    | logical_or_expr OR logical_and_expr
    ;

logical_and_expr
    : equality_expr
    | logical_and_expr AND equality_expr
    ;

equality_expr
    : relational_expr
    | equality_expr EQ relational_expr
    | equality_expr NE relational_expr
    ;

relational_expr
    : additive_expr
    | relational_expr '<' additive_expr
    | relational_expr '>' additive_expr
    | relational_expr LE additive_expr
    | relational_expr GE additive_expr
    ;

additive_expr
    : multiplicative_expr
    | additive_expr '+' multiplicative_expr
    | additive_expr '-' multiplicative_expr
    ;

multiplicative_expr
    : unary_expr
    | multiplicative_expr '*' unary_expr
    | multiplicative_expr '/' unary_expr
    | multiplicative_expr '%' unary_expr
    ;

unary_expr
    : '(' type_name ')' unary_expr
    | postfix_expr
    | INC unary_expr
    | DEC unary_expr
    | '&' unary_expr
    | '*' unary_expr
    | '+' unary_expr
    | '-' unary_expr                      %prec UMINUS
    | '!' unary_expr
    | SIZEOF unary_expr
    ;

type_name
    : decl_spec pointer_opt
    ;

postfix_expr
    : primary_expr
    | postfix_expr '[' expression ']'
    | postfix_expr '(' arg_list_opt ')'
    | postfix_expr INC
    | postfix_expr DEC
    ;

arg_list_opt
    : /* vazio */
    | arg_list
    ;

arg_list
    : assignment_expression
    | arg_list ',' assignment_expression
    ;

primary_expr
    : IDENT
    | INT_LIT
    | F_LIT
    | STRING
    | CHAR_LIT
    | '(' expression ')'
    ;
%%

/* Opcional:
module.exports = parser;
*/
