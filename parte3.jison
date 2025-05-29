/* =====================================================
 * compilador_c_completo.jison — Subconjunto ANSI-C p/ Jison
 *           (versão estendida c/ escopo + tipos)
 * =====================================================*/

/* ---------- 0.  Helpers / Runtime JS ---------------- */
%{
/* ─────── Symbol Table (escopo em pilha) ──────────── */
class SymbolTable {
  constructor () { this.scopes = [Object.create(null)]; }

  enter () { this.scopes.push(Object.create(null)); }
  exit  () { this.scopes.pop(); }

  declare (name, type, yy) {
    const cur = this.scopes[this.scopes.length - 1];
    if (cur[name])
      yy.parser.yyerror(`Variável redeclarada no mesmo bloco: '${name}'`);
    cur[name] = { type };
  }

  lookup (name) {
    for (let i = this.scopes.length - 1; i >= 0; --i) {
      const v = this.scopes[i][name];
      if (v) return v;
    }
    return null;
  }
}

const symtab = new SymbolTable();

/* ─────── Tipo ← token ─────────────────────────────── */
function specToType(tok) {
  switch (tok) {
    case 'CHAR_T':   return 'char';
    case 'INT_T':    return 'int';
    case 'FLOAT_T':  return 'float';
    case 'DOUBLE_T': return 'double';
    case 'VOID':     return 'void';
    default:         return 'unknown';
  }
}

/* ─────── Confere compatibilidade de tipos ─────────── */
function sameType(a, b, op, yy) {
  if (a !== b)
    yy.parser.yyerror(`Tipos incompatíveis (${a} ${op} ${b})`);
  return a;             /* propaga tipo resultante */
}

/* ─────── Confere atribuição ------------------------- */
function checkAssign(varType, exprType, yy) {
  if (varType !== exprType)
    yy.parser.yyerror(`Atribuição de ${exprType} em variável ${varType}`);
}
%}

/* ---------- 1. Léxico -------------------------------- */
%lex
%%       /* (léxico igual ao enviado, sem alterações) */
\s+                            /* ignorar espaços */
"//"[^\n]*                     /* comentário de linha */
\/\*[\s\S]*?\*\/               /* comentário multilinha */

"<"[A-Za-z0-9_\/\.]+">"        return 'HEADER';

/* palavras-reservadas */
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

%left ','
%right '=' PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN
%right '?'
%right ':'
%left OR
%left AND
%left '|'
%left '^'
%left '&'
%left EQ NE
%left '<' '>' LE GE
%left LSHIFT RSHIFT
%left '+' '-'
%left '*' '/' '%'
%right UMINUS

/* ---------- 3. Gramática ---------- */
%%

/* ---------- Programa / unidades externas ---------- */
program
  : external_list EOF                        { /* ok */ }
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

/* ---------- Pré-processador ---------- */
preprocessor_directive
  : '#' INCLUDE HEADER
  | '#' DEFINE ID define_value_opt
  ;

define_value_opt
  : expression
  | STR_LIT
  | /* vazio */
  ;

/* ---------- Declarações e funções ------------------ */
declaration
  : declaration_specifiers init_declarator_list ';'   { /* vars declaradas dentro de init_declarator */ }
  | declaration_specifiers ';'                        { /* só typedef ou semelhante */ }
  | ID init_declarator_list ';'                       { /* erro de sintaxe “tipo ausente” – deixo jison tratar */ }
  ;

function_definition
  : declaration_specifiers declarator compound_statement
      { /* todo: registrar função se quiser */ }
  ;

/* ---------- Especificadores ------------------------ */
declaration_specifiers
  : declaration_specifiers type_specifier
  | declaration_specifiers type_qualifier
  | declaration_specifiers storage_class_specifier
  | type_specifier
  | type_qualifier
  | storage_class_specifier
  ;

type_qualifier
  : CONST
  | VOLATILE
  ;

storage_class_specifier
  : AUTO
  | REGISTER
  | STATIC
  | EXTERN
  | TYPEDEF
  ;

/* ---- tipo     >>> captura yy.currentType ----------- */
type_specifier
  : CHAR_T        { yy.currentType = 'char';   }
  | INT_T         { yy.currentType = 'int';    }
  | FLOAT_T       { yy.currentType = 'float';  }
  | DOUBLE_T      { yy.currentType = 'double'; }
  | VOID          { yy.currentType = 'void';   }
  | struct_specifier
  | union_specifier
  | enum_specifier
  | SHORT_T
  | LONG_T
  | SIGNED
  | UNSIGNED
  ;

/* ----- struct / union / enum (sem alterações) ------ */
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

/* ---------- Declaradores --------------------------- */
init_declarator_list
  : init_declarator
  | init_declarator_list ',' init_declarator
  ;

init_declarator
  : declarator
      {                         /* int x;      */
        symtab.declare($1, yy.currentType, yy);
      }
  | declarator '=' initializer
      {                         /* int x = 5;  */
        symtab.declare($1, yy.currentType, yy);
        checkAssign(yy.currentType, $3.type, yy);
      }
  ;

declarator
  : pointer_opt direct_declarator    { $$ = $2; }   /* devolve nome */
  ;

pointer_opt
  : '*' pointer_opt
  | /* vazio */
  ;

direct_declarator
  : ID                               { $$ = $1; }   /* ← nome */
  | '(' declarator ')'               { $$ = $2; }
  | direct_declarator '[' constant_expression_opt ']'   { $$ = $1; }
  | direct_declarator '(' parameter_list_opt ')'       { $$ = $1; }
  ;

/* (parâmetros mantidos, sem ações) */
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
  : conditional_expression           { $$ = $1; }
  ;

initializer
  : assignment_expression            { $$ = $1; }
  | '{' initializer_list '}'         { $$ = { type:'void' }; } /* simplificado */
  ;

initializer_list
  : initializer
  | initializer_list ',' initializer
  ;

/* parâmetros (sem ações) */
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

/* ---------- Blocos & Escopos ----------------------- */
compound_statement
  : '{'            { symtab.enter(); }
    block_item_list_opt
    '}'            { symtab.exit();  }
  ;

block_item_list_opt
  : block_item_list
  | /* vazio */
  ;

block_item_list
  : block_item_list block_item
  | block_item
  ;

block_item
  : declaration
  | statement
  ;

/* ---------- Statements ----------------------------- */
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
  | SWITCH '(' expression ')' statement
  ;

iteration_statement
  : WHILE '(' expression ')' statement
  | DO statement WHILE '(' expression ')' ';'
  | FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
  | FOR '(' for_declaration  ';' expression_opt ';' expression_opt ')' statement
  ;

jump_statement
  : GOTO ID ';'
  | CONTINUE ';'
  | BREAK ';'
  | RETURN expression_opt ';'
  ;

/* ---------- EXPRESSÕES (com tipos) ----------------- */
expression
  : assignment_expression            { $$ = $1; }
  | expression ',' assignment_expression
      { $$ = $3; }    /* tipo resultante da última */
  ;

assignment_expression
  : conditional_expression           { $$ = $1; }
  | unary_expression assignment_operator assignment_expression
      {
        checkAssign($1.type, $3.type, yy);
        $$ = { type:$1.type };
      }
  ;

assignment_operator
  : '=' | PLUS_ASSIGN | MINUS_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | AND_ASSIGN | OR_ASSIGN | XOR_ASSIGN | LSHIFT_ASSIGN | RSHIFT_ASSIGN
  ;

conditional_expression
  : logical_or_expression
      { $$ = $1; }
  | logical_or_expression '?' expression ':' conditional_expression
      { sameType($1.type, $3.type, '?', yy); $$ = { type:$1.type }; }
  ;

logical_or_expression
  : logical_and_expression
      { $$ = $1; }
  | logical_or_expression OR logical_and_expression
      { sameType($1.type, $3.type, '||', yy); $$ = { type:'int' }; }
  ;

logical_and_expression
  : inclusive_or_expression
      { $$ = $1; }
  | logical_and_expression AND inclusive_or_expression
      { sameType($1.type, $3.type, '&&', yy); $$ = { type:'int' }; }
  ;

inclusive_or_expression
  : exclusive_or_expression
      { $$ = $1; }
  | inclusive_or_expression '|' exclusive_or_expression
      { sameType($1.type, $3.type, '|', yy);  $$ = { type:$1.type }; }
  ;

exclusive_or_expression
  : and_expression
      { $$ = $1; }
  | exclusive_or_expression '^' and_expression
      { sameType($1.type, $3.type, '^', yy);  $$ = { type:$1.type }; }
  ;

and_expression
  : equality_expression
      { $$ = $1; }
  | and_expression '&' equality_expression
      { sameType($1.type, $3.type, '&', yy);  $$ = { type:$1.type }; }
  ;

equality_expression
  : relational_expression
      { $$ = $1; }
  | equality_expression EQ relational_expression
      { sameType($1.type, $3.type, '==', yy); $$ = { type:'int' }; }
  | equality_expression NE relational_expression
      { sameType($1.type, $3.type, '!=', yy); $$ = { type:'int' }; }
  ;

relational_expression
  : shift_expression
      { $$ = $1; }
  | relational_expression '<' shift_expression
      { sameType($1.type, $3.type, '<', yy);  $$ = { type:'int' }; }
  | relational_expression '>' shift_expression
      { sameType($1.type, $3.type, '>', yy);  $$ = { type:'int' }; }
  | relational_expression LE shift_expression
      { sameType($1.type, $3.type, '<=', yy); $$ = { type:'int' }; }
  | relational_expression GE shift_expression
      { sameType($1.type, $3.type, '>=', yy); $$ = { type:'int' }; }
  ;

shift_expression
  : additive_expression
      { $$ = $1; }
  | shift_expression LSHIFT additive_expression
      { sameType($1.type, $3.type, '<<', yy); $$ = { type:$1.type }; }
  | shift_expression RSHIFT additive_expression
      { sameType($1.type, $3.type, '>>', yy); $$ = { type:$1.type }; }
  ;

additive_expression
  : multiplicative_expression
      { $$ = $1; }
  | additive_expression '+' multiplicative_expression
      { $$ = { type:sameType($1.type, $3.type, '+', yy) }; }
  | additive_expression '-' multiplicative_expression
      { $$ = { type:sameType($1.type, $3.type, '-', yy) }; }
  ;

multiplicative_expression
  : cast_expression
      { $$ = $1; }
  | multiplicative_expression '*' cast_expression
      { $$ = { type:sameType($1.type, $3.type, '*', yy) }; }
  | multiplicative_expression '/' cast_expression
      { $$ = { type:sameType($1.type, $3.type, '/', yy) }; }
  | multiplicative_expression '%' cast_expression
      { $$ = { type:sameType($1.type, $3.type, '%', yy) }; }
  ;

cast_expression
  : unary_expression                 { $$ = $1; }
  | '(' type_name ')' cast_expression { $$ = $4; } /* cast ignora */
  ;

unary_expression
  : postfix_expression               { $$ = $1; }
  | INC unary_expression             { $$ = $2; }
  | DEC unary_expression             { $$ = $2; }
  | unary_operator cast_expression   { $$ = $2; }
  | SIZEOF unary_expression          { $$ = { type:'int' }; }
  | SIZEOF '(' type_name ')'         { $$ = { type:'int' }; }
  ;

unary_operator
  : '&'
  | '*'
  | '+'
  | '-'
  | '!'
  ;

/* ---------- postfix / primary ---------------------- */
postfix_expression
  : primary_expression               { $$ = $1; }
  | postfix_expression '[' expression ']'           { $$ = { type:$1.type }; }
  | postfix_expression '(' argument_expression_list_opt ')' { $$ = $1; }
  | postfix_expression '.' ID        { $$ = { type:'unknown' }; }
  | postfix_expression ARROW ID      { $$ = { type:'unknown' }; }
  | postfix_expression INC           { $$ = $1; }
  | postfix_expression DEC           { $$ = $1; }
  ;

argument_expression_list_opt
  : argument_expression_list
  | /* vazio */
  ;

argument_expression_list
  : assignment_expression
  | argument_expression_list ',' assignment_expression
  ;

/* -------- primary: onde resolvemos variáveis ------- */
primary_expression
  : ID
      {
        const v = symtab.lookup($1);
        if (!v) yy.parser.yyerror(`Variável não declarada: '${$1}'`);
        $$ = { type:v.type, name:$1 };
      }
  | INT_LIT  { $$ = { type:'int'   }; }
  | F_LIT    { $$ = { type:'float' }; }
  | CHAR_LIT { $$ = { type:'char'  }; }
  | STR_LIT  { $$ = { type:'char*' }; }
  | '(' expression ')'              { $$ = $2; }
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

for_declaration
  : declaration_specifiers init_declarator_list
  ;

%%

/* ---------- 4. Epílogo (facultativo) --------------- */
if (typeof module !== 'undefined') module.exports = parser;