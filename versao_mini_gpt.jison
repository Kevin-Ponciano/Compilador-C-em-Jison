%{
class SymbolTable {
  constructor () { this.scopes = [Object.create(null)]; }
  enter ()  { this.scopes.push(Object.create(null)); }
  exit ()   { this.scopes.pop(); }
  declare (name, type, yy) {
    const cur = this.scopes[this.scopes.length - 1];
    if (cur[name]) yy.parser.yyerror(`Variável redeclarada: ${name}`);
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

function sameType(a,b,op,yy){ if(a!==b) yy.parser.yyerror(`Tipos incompatíveis (${a} ${op} ${b})`); return a; }
function assignOk(vt,et,yy){ if(vt!==et) yy.parser.yyerror(`Atribuição de ${et} em var ${vt}`); }
%}

/* ---------- 1. Léxico -------------------------------- */
%lex
%%
\s+                          /* ignora espaço */
"//"[^\n]*                  /* comentário linha */
\/\*[\s\S]*?\*\/            /* comentário bloco */

"int"\b        return 'INT_T';
"float"\b      return 'FLOAT_T';
"void"\b       return 'VOID';
"return"\b     return 'RETURN';
"if"\b         return 'IF';
"else"\b       return 'ELSE';
"while"\b      return 'WHILE';
"{"            return '{';
"}"            return '}';
"("            return '(';
")"            return ')';
";"            return ';';
"="            return '=';
"=="           return 'EQ';
"+"            return '+';
"-"            return '-';
"*"            return '*';
"/"            return '/';
"<"            return '<';
">"            return '>';
[0-9]+\.[0-9]+  return 'F_LIT';
[0-9]+           return 'INT_LIT';
[A-Za-z_][A-Za-z0-9_]* return 'ID';
<<EOF>>          return 'EOF';
/lex

/* ---------- 2. Precedência ---------- */
%start program
%token INT_T FLOAT_T VOID IF ELSE WHILE RETURN
%token ID INT_LIT F_LIT EQ
%left '+' '-'
%left '*' '/'
%left '<' '>'
%right '='

%%
program
  : ext_list EOF { return $1; }
  ;

ext_list
  : ext_list ext { $$ = $1; }
  | ext         { $$ = [$1]; }
  ;

ext
  : func_def
  | declaration
  ;

func_def
  : type_spec ID '(' ')' compound {
      $$ = {kind:'func',type:$1,name:$2,body:$5};
    }
  ;

type_spec
  : INT_T   { $$ = 'int';   }
  | FLOAT_T { $$ = 'float'; }
  | VOID    { $$ = 'void';  }
  ;

declaration
  : type_spec declarator_list ';' { $$ = {kind:'decl',type:$1,declarators:$2}; }
  ;

declarator_list
  : declarator                     { $$ = [$1]; }
  | declarator_list ',' declarator { $$ = $1; $$.push($3);} 
  ;

declarator
  : ID {
      symtab.declare($1, yy.$type || 'int', yy);
      $$ = {name:$1};
    }
  ;

compound
  : '{' { symtab.enter(); } stmt_list_opt '}' { symtab.exit(); $$ = $3; }
  ;

stmt_list_opt
  : stmt_list
  | /* vazio */ { $$ = []; }
  ;

stmt_list
  : stmt_list statement { $$ = $1; $$.push($2); }
  | statement           { $$ = [$1]; }
  ;

statement
  : declaration                    { $$ = $1; }
  | expr_stmt                      { $$ = $1; }
  | compound                       { $$ = $1; }
  | sel_stmt                       { $$ = $1; }
  | iter_stmt                      { $$ = $1; }
  | jump_stmt                      { $$ = $1; }
  ;

expr_stmt
  : expr_opt ';' { $$ = $1; }
  ;

expr_opt
  : expression
  | /* vazio */ { $$ = null; }
  ;

sel_stmt
  : IF '(' expression ')' statement {
      $$ = {kind:'if',cond:$3,then:$5};
    }
  | IF '(' expression ')' statement ELSE statement {
      $$ = {kind:'ifelse',cond:$3,then:$5,else:$7};
    }
  ;

iter_stmt
  : WHILE '(' expression ')' statement {
      $$ = {kind:'while',cond:$3,body:$5};
    }
  ;

jump_stmt
  : RETURN expression ';' { $$ = {kind:'return',value:$2}; }
  ;

/* ---------- EXPRESSÕES ---------- */
expression
  : assignment
  ;

assignment
  : ID '=' assignment {
      const v = symtab.lookup($1);
      if(!v) yy.parser.yyerror(`Variável não declarada: ${$1}`);
      const rType = $3.type;
      assignOk(v.type,rType,yy);
      $$ = {type:v.type};
    }
  | equality                   { $$ = $1; }
  ;

equality
  : additive
  | additive EQ additive {
      sameType($1.type,$3.type,'==',yy);
      $$ = {type:'int'};
    }
  ;

additive
  : term
  | additive '+' term {
      $$ = {type:sameType($1.type,$3.type,'+',yy)};
    }
  | additive '-' term {
      $$ = {type:sameType($1.type,$3.type,'-',yy)};
    }
  ;

term
  : factor
  | term '*' factor { $$ = {type:sameType($1.type,$3.type,'*',yy)}; }
  | term '/' factor { $$ = {type:sameType($1.type,$3.type,'/',yy)}; }
  ;

factor
  : ID {
      const v = symtab.lookup($1);
      if(!v) yy.parser.yyerror(`Variável não declarada: ${$1}`);
      $$ = {type:v.type};
    }
  | INT_LIT { $$ = {type:'int'}; }
  | F_LIT   { $$ = {type:'float'}; }
  | '(' expression ')' { $$ = $2; }
  ;

%%
