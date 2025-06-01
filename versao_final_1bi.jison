/* =====================================================
 * compilador_c_completo.jison — Subconjunto ANSI‑C para Jison
 * =====================================================*/

%{
/* ------------ CONTROLE DE ESCOPO & TIPO ------------ */
let currentScope = 0;            // 0 = global
let symbolTable  = [];           // { name, type, scope }

/* escopo vem do léxico (enterScope/exitScope) */
function enterScope ()  { currentScope++; }
function exitScope  ()  {
  currentScope--;
  symbolTable = symbolTable.filter(s => s.scope <= currentScope);
}

/* ----------- tabela de símbolos ------------ */
function addSymbol(name, type)   { symbolTable.push({name,type,scope:currentScope}); }
function findSymbol(name) {
  for (let i = symbolTable.length-1; i >= 0; i--) {
    const s = symbolTable[i];
    if (s.name === name && s.scope <= currentScope) return s;
  }
  throw new Error(`Variável '${name}' não declarada no escopo atual`);
}

/* ----------- verificação de tipos ------------ */
function typeEquals(t1, t2) {
  if (t1 === t2) return true;
  const num  = v => ['int','char'].includes(v);          // inteiros
  const real = v => ['float','double'].includes(v);      // reais
  return (num(t1)  && real(t2)) || (num(t2) && real(t1));  // promoção
}

/* helper genérico para binários */
function checkBin(op, lhs, rhs) {
  if (!typeEquals(lhs.type, rhs.type))
    throw new Error(`Tipos incompatíveis em '${op}': ${lhs.type} vs ${rhs.type}`);

  /* ↓ resultado: aritméticos → numérico; lógicos/comparações/bitwise → int */
  const toInt = ['<','>','<=','>=','==','!=','&&','||','&','|','^','<<','>>'];
  return { type: toInt.includes(op) ? 'int' : lhs.type };
}

/* tipo da declaração corrente (usado em init_declarator) */
let __tipoDecl = null;
%}




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
"|"            return '|';
"^"            return '^';

/* operadores e símbolos simples */
"!"            return '!';
"?"            return '?';
":"            return ':';
"["            return '[';
"]"            return ']';
"{"            {  enterScope(); return 'LBRACE'; }
"}"            {  exitScope(); return 'RBRACE'; }
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
// %token INCLUDE_DIRECTIVE

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

// include_dir
//   : '#' INCLUDE HEADER  /* removido ';'? */
//   ;

// include_dir
//   : INCLUDE_DIRECTIVE ';'?
//   ;

declaration
  : declaration_specifiers init_declarator_list ';'
  | declaration_specifiers ';' 
  | ID init_declarator_list ';'
  ;

function_definition
  : declaration_specifiers declarator compound_statement
  ;


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


type_specifier
  : CHAR_T      { __tipoDecl = 'char';   }
  | INT_T       { __tipoDecl = 'int';    }
  | FLOAT_T     { __tipoDecl = 'float';  }
  | DOUBLE_T    { __tipoDecl = 'double'; }
  | VOID        { __tipoDecl = 'void';   }
  | struct_specifier
  | union_specifier
  | enum_specifier
  | SHORT_T           /*  short            */
  | LONG_T            /*  long             */
  | SIGNED            /*  signed           */
  | UNSIGNED          /*  unsigned         */
  ;


struct_specifier
  : STRUCT ID LBRACE struct_declaration_list RBRACE
  | STRUCT LBRACE struct_declaration_list RBRACE
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
  : declarator                       /* sem inicialização */
      { addSymbol($1, __tipoDecl); }
  | declarator '=' initializer       /* com inicialização */
      { addSymbol($1, __tipoDecl); }
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
  | LBRACE initializer_list RBRACE
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


/* ---------- novo bloco ------------------------------ */

compound_statement
  : LBRACE block_item_list_opt RBRACE
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
  : declaration    /* int x;  enum Color c = RED; … */
  | statement      /* qualquer instrução            */
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
  : expression        { $$ = $1; }      /* devolve o tipo que veio da expressão */
  | /* vazio */       { $$ = { type:'void' }; }   /* não há expressão → tipo void */
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

/* ------------- EXPRESSÕES ------------- */

expression
  : assignment_expression
  | expression ',' assignment_expression
  ;

/* ---------- ATRIBUIÇÃO ---------- */
assignment_expression
  : unary_expression '=' assignment_expression      { $$ = checkBin('=',  $1, $3); }
  | unary_expression assignment_operator assignment_expression
      { $$ = checkBin($2, $1, $3); }                /* += -= *= … */
  | conditional_expression
  ;

assignment_operator
  : '=' | PLUS_ASSIGN | MINUS_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | AND_ASSIGN | OR_ASSIGN  | XOR_ASSIGN | LSHIFT_ASSIGN | RSHIFT_ASSIGN
  ;

/* ---------- OPERADOR TERNÁRIO ---------- */
conditional_expression
  : logical_or_expression
  | logical_or_expression '?' expression ':' conditional_expression
      { $$ = { type: $3.type }; /* condição → int, mas resultado herda braço */ }
  ;

/* ---------- OU LÓGICO ---------- */
logical_or_expression
  : logical_and_expression
  | logical_or_expression OR logical_and_expression
      { $$ = checkBin('||', $1, $3); }
  ;

/* ---------- E LÓGICO ---------- */
logical_and_expression
  : inclusive_or_expression
  | logical_and_expression AND inclusive_or_expression
      { $$ = checkBin('&&', $1, $3); }
  ;

/* ---------- OR BIT-A-BIT ---------- */
inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression '|' exclusive_or_expression
      { $$ = checkBin('|', $1, $3); }
  ;

/* ---------- XOR BIT-A-BIT ---------- */
exclusive_or_expression
  : and_expression
  | exclusive_or_expression '^' and_expression
      { $$ = checkBin('^', $1, $3); }
  ;

/* ---------- AND BIT-A-BIT ---------- */
and_expression
  : equality_expression
  | and_expression '&' equality_expression
      { $$ = checkBin('&', $1, $3); }
  ;

/* ---------- IGUALDADE ---------- */
equality_expression
  : relational_expression
  | equality_expression EQ relational_expression
      { $$ = checkBin('==', $1, $3); }
  | equality_expression NE relational_expression
      { $$ = checkBin('!=', $1, $3); }
  ;

/* ---------- RELACIONAIS ---------- */
relational_expression
  : shift_expression
  | relational_expression '<'  shift_expression  { $$ = checkBin('<',  $1, $3); }
  | relational_expression '>'  shift_expression  { $$ = checkBin('>',  $1, $3); }
  | relational_expression LE   shift_expression  { $$ = checkBin('<=', $1, $3); }
  | relational_expression GE   shift_expression  { $$ = checkBin('>=', $1, $3); }
  ;

/* ---------- SHIFT ---------- */
shift_expression
  : additive_expression
  | shift_expression LSHIFT additive_expression   { $$ = checkBin('<<', $1, $3); }
  | shift_expression RSHIFT additive_expression   { $$ = checkBin('>>', $1, $3); }
  ;

/* ---------- +  - ---------- */
additive_expression
  : multiplicative_expression
  | additive_expression '+' multiplicative_expression { $$ = checkBin('+', $1, $3); }
  | additive_expression '-' multiplicative_expression { $$ = checkBin('-', $1, $3); }
  ;

/* ---------- *  /  % ---------- */
multiplicative_expression
  : cast_expression
  | multiplicative_expression '*' cast_expression   { $$ = checkBin('*', $1, $3); }
  | multiplicative_expression '/' cast_expression   { $$ = checkBin('/', $1, $3); }
  | multiplicative_expression '%' cast_expression   { $$ = checkBin('%', $1, $3); }
  ;

/* ---------- CAST / UNÁRIO ---------- */
cast_expression
  : unary_expression
  | '(' type_name ')' cast_expression   { $$ = $4; }   /* type-cast ignora verificação aqui */
  ;

unary_expression
  : postfix_expression
  | INC unary_expression   { $$ = $2; }
  | DEC unary_expression   { $$ = $2; }
  | unary_operator cast_expression      { $$ = $2; }
  | SIZEOF unary_expression             { $$ = { type:'int' }; }
  | SIZEOF '(' type_name ')'            { $$ = { type:'int' }; }
  ;

/* ---------- OPERADORES UNÁRIOS ---------- */
unary_operator
  : '&' | '*' | '+' | '-' | '!'
  ;

/* ---------- POSTFIX / PRIMÁRIOS ---------- */
postfix_expression
  : primary_expression
  | postfix_expression '[' expression ']'                  { $$ = { type: $1.type }; }
  | postfix_expression '(' argument_expression_list_opt ')' { $$ = { type: $1.type }; }
  | postfix_expression '.' ID                              { $$ = { type: $1.type }; }
  | postfix_expression ARROW ID                            { $$ = { type: $1.type }; }
  | postfix_expression INC                                 { $$ = { type: $1.type }; }
  | postfix_expression DEC                                 { $$ = { type: $1.type }; }
  ;

argument_expression_list_opt
  : argument_expression_list
  | /* vazio */
  ;

argument_expression_list
  : assignment_expression
  | argument_expression_list ',' assignment_expression
  ;

/* ---------- PRIMÁRIOS ---------- */
primary_expression
  : ID        { const s = findSymbol($1); $$ = { type: s.type }; }
  | INT_LIT   { $$ = { type:'int'    }; }
  | F_LIT     { $$ = { type:'float'  }; }
  | CHAR_LIT  { $$ = { type:'char'   }; }
  | STR_LIT   { $$ = { type:'char*'  }; }
  | '(' expression ')' { $$ = $2; }
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
  : UNION ID LBRACE struct_declaration_list RBRACE
  | UNION LBRACE struct_declaration_list RBRACE
  | UNION ID
  ;

enum_specifier
  : ENUM ID LBRACE enumerator_list RBRACE
  | ENUM LBRACE enumerator_list RBRACE
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
  : declaration_specifiers init_declarator_list   /* ← obrigatório */
  ;

%%
