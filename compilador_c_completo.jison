/* =====================================================
 * compilador_c_completo.jison — Subconjunto ANSI‑C para Jison
 * =====================================================*/

%{
/* ------------ CONTROLE DE ESCOPO & TIPO ------------ */
let currentScope = 0;            // 0 = global
let allSymbols = [];
let symbolTable  = [];           // { name, type, scope }
let semanticErrors = [];
let tac = [];                   // lista do código intermediário
let tempCount = 0;              // contador para temporários

function printAST(node, indent = 0) {
  if (node === null || node === undefined) return;
  const pad = '  '.repeat(indent);

  // Se for string ou número, imprime como token
  if (typeof node === 'string' || typeof node === 'number') {
    console.log(`${pad}Token: ${node}`);
    return;
  }

  // Se for array, imprime cada elemento
  if (Array.isArray(node)) {
    node.forEach(child => printAST(child, indent));
    return;
  }

  // Se tiver propriedade 'op' (nó AST de expressão)
  if (node.op) {
    console.log(`${pad}Op: ${node.op}${node.lex ? ` (lex: ${node.lex})` : ''}`);
    if (node.left) printAST(node.left, indent + 1);
    if (node.right) printAST(node.right, indent + 1);
    return;
  }

  // Se tiver tipo e filhos (objeto composto)
  if (node.type) {
    console.log(`${pad}Type: ${node.type}`);

    // Imprime filhos, se existirem
    if (node.children && node.children.length > 0) {
      node.children.forEach(child => printAST(child, indent + 1));
    }

    // Imprime propriedades extras além de type e children
    for (const key in node) {
      if (key !== 'type' && key !== 'children' && node[key] && typeof node[key] !== 'object') {
        console.log(`${pad}  ${key}: ${node[key]}`);
      }
    }
    return;
  }

  // Caso genérico: imprime JSON
  console.log(`${pad}Node: ${JSON.stringify(node)}`);
}

function newTemp() {
  const t = `t${++tempCount}`;
  console.log(`[newTemp] Gerado temporário: ${t}`);
  return t;
}

// Nó da árvore sintática
function node(op, left, right, lex) {
  console.log(`[node] Criando nó AST: op='${op}', lex='${lex || ""}'`);
  return { op, left, right, lex };
}

// Geração do código de três endereços a partir da AST
function emitTAC(ast) {
  if (!ast) return null;

  // Nós folha
  if (['ID', 'INT', 'FLOAT', 'CHAR', 'STR'].includes(ast.op)) {
    return ast.lex;
  }

  // Recursão nos filhos, pode ser null ou undefined
  const a = emitTAC(ast.left);
  const b = emitTAC(ast.right);

  const leftVal = a !== undefined && a !== null ? a : '';
  const rightVal = b !== undefined && b !== null ? b : '';

  const t = newTemp();

  // Trate operadores unários / chamadas especiais
  if (!rightVal) {
    // Exemplo para chamada sem argumentos ou operadores unários
    tac.push(`${t} = ${ast.op} ${leftVal}`.trim());
  } else {
    tac.push(`${t} = ${leftVal} ${ast.op} ${rightVal}`.trim());
  }

  return t;
}



/* Escopo vem do léxico (enterScope/exitScope) */
function enterScope ()  {
  currentScope++;
  console.log(`[enterScope] Entrando no escopo ${currentScope}`);
}
function exitScope  ()  {
  console.log(`[exitScope] Saindo do escopo ${currentScope}`);
  currentScope--;
  symbolTable = symbolTable.filter(s => s.scope <= currentScope);
}

/* Função para registrar erros */
function reportError(msg) {
  semanticErrors.push(msg);
  console.warn(`[reportError] ${msg}`);
}

/* ----------- tabela de símbolos ------------ */
function addSymbol(name, type) {
  // Verifica se já existe variável com mesmo nome no escopo atual
  const exists = symbolTable.some(s => s.name === name && s.scope === currentScope);
  if (exists) {
    reportError(`Variável '${name}' já declarada no escopo ${currentScope}`);
  } else {
    symbolTable.push({name, type, scope: currentScope});
    allSymbols.push({name, type, scope: currentScope});
    console.log(`[addSymbol] Adicionada variável '${name}' do tipo '${type}' no escopo ${currentScope}`);
  }
}

/* Busca variável visível mais interna, sem lançar erro */
function findSymbol(name) {
  for (let i = symbolTable.length - 1; i >= 0; --i) {
    const s = symbolTable[i];
    if (s.name === name && s.scope <= currentScope) {
      console.log(`[findSymbol] Encontrado símbolo '${name}' com tipo '${s.type}' no escopo ${s.scope}`);
      return s;
    }
  }
  reportError(`Variável '${name}' não declarada no escopo atual (${currentScope})`);
  return { name, type: 'undefined', scope: currentScope };
}

/* ----------- verificação de tipos ------------ */
function typeEquals(t1, t2) {
  if (!t1 || !t2) {
    console.warn(`[typeEquals] Tipo indefinido comparado: '${t1}' vs '${t2}'`);
    return false;
  }
  if (t1 === 'undefined' || t2 === 'undefined') {
    console.warn(`[typeEquals] Tipo 'undefined' envolvido: '${t1}' vs '${t2}'`);
    return false;
  }

  if (t1.endsWith('*') && t2.endsWith('*')) {
    console.log(`[typeEquals] Comparando ponteiros: '${t1}' vs '${t2}' -> igual`);
    return true; // aceitando equivalência simplificada para ponteiros
  }

  if (t1 === t2) {
    console.log(`[typeEquals] Tipos iguais: '${t1}'`);
    return true;
  }

  const num  = v => ['int','char'].includes(v);
  const real = v => ['float','double'].includes(v);
  const res = (num(t1) && real(t2)) || (num(t2) && real(t1));
  console.log(`[typeEquals] Promoção numérica entre '${t1}' e '${t2}': ${res}`);
  return res;
}

/* helper genérico para binários */
function checkBin(op, lhs, rhs) {
  if (!lhs || !rhs) {
    reportError(`Operandos indefinidos na operação '${op}'`);
    return { type: 'undefined' };
  }
  if (!typeEquals(lhs.type, rhs.type)) {
    reportError(`Tipos incompatíveis em '${op}': ${lhs.type} vs ${rhs.type}`);
    return { type: 'undefined' };
  }
  console.log(`[checkBin] Operação '${op}' com tipos compatíveis: ${lhs.type} vs ${rhs.type}`);

  const toInt = ['<','>','<=','>=','==','!=','&&','||','&','|','^','<<','>>'];
  const resultType = toInt.includes(op) ? 'int' : lhs.type;
  console.log(`[checkBin] Tipo resultante: ${resultType}`);
  return { type: resultType };
}

/* tipo da declaração corrente (usado em init_declarator) */
let __tipoDecl = null;

/* Função para obter erros semânticos */
function getSemanticErrors() {
  return semanticErrors;
}


// Declarar funções e macros padrão da libc para evitar erros
addSymbol('printf', 'function');
addSymbol('scanf', 'function');
addSymbol('malloc', 'void*');
addSymbol('free', 'function');
addSymbol('exit', 'function');
addSymbol('NULL', 'void*');
//addSymbol('sizeof', 'function'); // Função para 'sizeof'
addSymbol('memset', 'function'); // Exemplo de outra função de C

// Palavras reservadas em C
addSymbol('auto', 'keyword');
addSymbol('break', 'keyword');
addSymbol('case', 'keyword');
addSymbol('char', 'keyword');
addSymbol('const', 'keyword');
addSymbol('continue', 'keyword');
addSymbol('default', 'keyword');
addSymbol('do', 'keyword');
addSymbol('double', 'keyword');
addSymbol('else', 'keyword');
addSymbol('enum', 'keyword');
addSymbol('extern', 'keyword');
addSymbol('float', 'keyword');
addSymbol('for', 'keyword');
addSymbol('goto', 'keyword');
addSymbol('if', 'keyword');
addSymbol('int', 'keyword');
addSymbol('long', 'keyword');
addSymbol('register', 'keyword');
addSymbol('return', 'keyword');
addSymbol('short', 'keyword');
addSymbol('signed', 'keyword');
addSymbol('sizeof', 'keyword');
addSymbol('static', 'keyword');
addSymbol('struct', 'keyword');
addSymbol('switch', 'keyword');
addSymbol('typedef', 'keyword');
addSymbol('union', 'keyword');
addSymbol('unsigned', 'keyword');
addSymbol('void', 'keyword');
addSymbol('volatile', 'keyword');
addSymbol('while', 'keyword');
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
  : external_list EOF
      {
        console.log('\nTabela de Símbolos Final:');
        allSymbols.forEach(s => console.log(`${s.name} : ${s.type} : escopo ${s.scope}`));

        console.log('\nCódigo de Três Endereços:');
        tac.forEach(line => console.log(line));

        if (semanticErrors.length > 0) {
          console.log('\nErros Semânticos encontrados:');
          semanticErrors.forEach(e => console.log('- ' + e));
        } else {
          console.log('\nNenhum erro semântico encontrado.');
        }

        console.log('\nÁrvore Sintática (AST) do programa:');
        printAST($1);  // $1 é external_list com filhos
      }
  ;


external_list
  : external_list external
      {
        $$ = { type: 'ExternalList', children: $1.children.concat([$2]) };
      }
  | external
      {
        $$ = { type: 'ExternalList', children: [$1] };
      }
  ;



external
  : preprocessor_directive
  | function_definition
  | declaration
  ;

preprocessor_directive
  : '#' INCLUDE HEADER
      {
        $$ = { type: 'PreprocessorInclude', value: $3 };
      }
  | '#' DEFINE ID define_value_opt
      {
        $$ = { type: 'PreprocessorDefine', id: $3, value: $4 };
      }
  ;


define_value_opt
  : expression
  | STR_LIT
  | /* vazio */
  ;

declaration
  : declaration_specifiers init_declarator_list ';'
      {
        $$ = { type: 'Declaration', specifiers: $1, initDeclarators: $2 };
      }
  | declaration_specifiers ';'
      {
        $$ = { type: 'Declaration', specifiers: $1 };
      }
  | ID init_declarator_list ';'
      {
        $$ = { type: 'Declaration', specifiers: $1, initDeclarators: $2 };
      }
  ;


function_definition
  : declaration_specifiers declarator compound_statement
  ;


external_list
  : external_list external
      {
        $$ = { type: 'ExternalList', children: $1.children.concat([$2]) };
      }
  | external
      {
        $$ = { type: 'ExternalList', children: [$1] };
      }
  ;

declaration_specifiers
  : declaration_specifiers type_specifier
      { $$ = { type: 'DeclarationSpecifiers', children: $1.children.concat([$2]) }; }
  | declaration_specifiers type_qualifier
      { $$ = { type: 'DeclarationSpecifiers', children: $1.children.concat([$2]) }; }
  | declaration_specifiers storage_class_specifier
      { $$ = { type: 'DeclarationSpecifiers', children: $1.children.concat([$2]) }; }
  | type_specifier
      { $$ = { type: 'DeclarationSpecifiers', children: [$1] }; }
  | type_qualifier
      { $$ = { type: 'DeclarationSpecifiers', children: [$1] }; }
  | storage_class_specifier
      { $$ = { type: 'DeclarationSpecifiers', children: [$1] }; }
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
  : CHAR_T      { $$ = { type: 'char' }; __tipoDecl = 'char'; }
  | INT_T       { $$ = { type: 'int' }; __tipoDecl = 'int'; }
  | FLOAT_T     { $$ = { type: 'float' }; __tipoDecl = 'float'; }
  | DOUBLE_T    { $$ = { type: 'double' }; __tipoDecl = 'double'; }
  | VOID        { $$ = { type: 'void' }; __tipoDecl = 'void'; }
  | struct_specifier
  | union_specifier
  | enum_specifier
  | SHORT_T           { $$ = { type: 'short' }; }
  | LONG_T            { $$ = { type: 'long' }; }
  | SIGNED            { $$ = { type: 'signed' }; }
  | UNSIGNED          { $$ = { type: 'unsigned' }; }
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
  : declarator
      {
        addSymbol($1.name, __tipoDecl + $1.pointer);
        $$ = { type: 'InitDeclarator', name: $1.name, pointer: $1.pointer };
      }
  | declarator '=' initializer
      {
        addSymbol($1.name, __tipoDecl + $1.pointer);
        $$ = { type: 'InitDeclarator', name: $1.name, pointer: $1.pointer, init: $3 };
      }
  ;

declarator
  : pointer_opt direct_declarator
      { $$ = { pointer: $1, name: $2 }; }
  ;




pointer_opt
  : '*' pointer_opt
      { $$ = '*' + ($2 || ''); }   /* concatena ponteiros, ex: '**' */
  | /* vazio */
      { $$ = ''; }
  ;

direct_declarator
  : ID
      { $$ = $1; }
  | '(' declarator ')'
      { $$ = $2.name; }
  | direct_declarator '[' constant_expression_opt ']'
      { $$ = $1.name; }
  | direct_declarator '(' parameter_list_opt ')'
      { $$ = $1.name; }
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
  : unary_expression '=' assignment_expression      
    { 
      $$ = checkBin('=',  $1, $3);
      $$ .ast = node('=', $1.ast, $3.ast);
      emitTAC($$.ast);
     }
  | unary_expression assignment_operator assignment_expression
    {
      $$ = checkBin($2, $1, $3);
      $$ .ast = node($2, $1.ast, $3.ast);
      emitTAC($$.ast);
    }
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
      { 
        $$ = checkBin('||', $1, $3); 
        $$ .ast = node('||', $1.ast, $3.ast);
      }
  ;

/* ---------- E LÓGICO ---------- */
logical_and_expression
  : inclusive_or_expression
  | logical_and_expression AND inclusive_or_expression
      { 
        $$ = checkBin('&&', $1, $3);
        $$ .ast = node('||', $1.ast, $3.ast);
      }
  ;

/* ---------- OR BIT-A-BIT ---------- */
inclusive_or_expression
  : exclusive_or_expression
  | inclusive_or_expression '|' exclusive_or_expression
      {
        $$ = checkBin('|', $1, $3);
        $$ .ast = node('|', $1.ast, $3.ast);
      }
  ;

/* ---------- XOR BIT-A-BIT ---------- */
exclusive_or_expression
  : and_expression
  | exclusive_or_expression '^' and_expression
      {
        $$ = checkBin('^', $1, $3);
        $$ .ast = node('^', $1.ast, $3.ast);
      }
  ;

/* ---------- AND BIT-A-BIT ---------- */
and_expression
  : equality_expression
  | and_expression '&' equality_expression
       {
        $$ = checkBin('&', $1, $3);
        $$ .ast = node('&', $1.ast, $3.ast);
      }
  ;

/* ---------- IGUALDADE ---------- */
equality_expression
  : relational_expression
  | equality_expression EQ relational_expression
      {
        $$ = checkBin('==', $1, $3);
        $$ .ast = node('==', $1.ast, $3.ast);
      }
  | equality_expression NE relational_expression
      {
        $$ = checkBin('!=', $1, $3);
        $$ .ast = node('!=', $1.ast, $3.ast);
      }
  ;

/* ---------- RELACIONAIS ---------- */
relational_expression
  : shift_expression
  | relational_expression '<' shift_expression
      {
        $$ = checkBin('<', $1, $3);
        $$ .ast = node('<', $1.ast, $3.ast);
      }
  | relational_expression '>' shift_expression
      {
        $$ = checkBin('>', $1, $3);
        $$ .ast = node('>', $1.ast, $3.ast);
      }
  | relational_expression LE shift_expression
      {
        $$ = checkBin('<=', $1, $3);
        $$ .ast = node('<=', $1.ast, $3.ast);
      }
  | relational_expression GE shift_expression
      {
        $$ = checkBin('>=', $1, $3);
        $$ .ast = node('>=', $1.ast, $3.ast);
      }
  ;

/* ---------- SHIFT ---------- */
shift_expression
  : additive_expression
  | shift_expression LSHIFT additive_expression
      {
        $$ = checkBin('<<', $1, $3);
        $$ .ast = node('<<', $1.ast, $3.ast);
      }
  | shift_expression RSHIFT additive_expression
      {
        $$ = checkBin('>>', $1, $3);
        $$ .ast = node('>>', $1.ast, $3.ast);
      }
  ;

/* ---------- +  - ---------- */
additive_expression
  : multiplicative_expression
  | additive_expression '+' multiplicative_expression
      {
        $$ = checkBin('+', $1, $3);
        $$ .ast = node('+', $1.ast, $3.ast);
      }
  | additive_expression '-' multiplicative_expression
      {
        $$ = checkBin('-', $1, $3);
        $$ .ast = node('-', $1.ast, $3.ast);
      }
  ;

/* multiplicação, divisão, módulo */
multiplicative_expression
  : cast_expression
  | multiplicative_expression '*' cast_expression
      {
        $$ = checkBin('*', $1, $3);
        $$ .ast = node('*', $1.ast, $3.ast);
      }
  | multiplicative_expression '/' cast_expression
      {
        $$ = checkBin('/', $1, $3);
        $$ .ast = node('/', $1.ast, $3.ast);
      }
  | multiplicative_expression '%' cast_expression
      {
        $$ = checkBin('%', $1, $3);
        $$ .ast = node('%', $1.ast, $3.ast);
      }
  ;

/* ---------- CAST / UNÁRIO ---------- */
cast_expression
  : unary_expression
  | '(' type_name ')' cast_expression   { $$ = $4; }   /* type-cast ignora verificação aqui */
  ;

unary_expression
  : postfix_expression
  | INC unary_expression
      { $$ = $2; }
  | DEC unary_expression
      { $$ = $2; }
  | unary_operator cast_expression
      { $$ = $2; }
  | SIZEOF unary_expression
      { $$ = { type: 'int' }; }
  | SIZEOF '(' type_name ')'
      { $$ = { type: 'int' }; }
  ;

/* ---------- OPERADORES UNÁRIOS ---------- */
unary_operator
  : '&' | '*' | '+' | '-' | '!'
  ;

/* ---------- POSTFIX / PRIMÁRIOS ---------- */
postfix_expression
  : primary_expression
      { $$ = { type: $1.type, ast: $1.ast }; }
  | postfix_expression '[' expression ']'
      {
        let baseType = $1.type;
        if (baseType.endsWith('*')) {
          baseType = baseType.slice(0, -1).trim();
        }
        $$ = { type: baseType, ast: node('[]', $1.ast, $3.ast) };
      }
  | postfix_expression '(' argument_expression_list_opt ')'
      { $$ = { type: $1.type, ast: node('call', $1.ast, $3 ? $3.ast : null) }; }
  | postfix_expression '.' ID
      { $$ = { type: $1.type, ast: node('.', $1.ast, null, $3) }; }
  | postfix_expression ARROW ID
      { $$ = { type: $1.type, ast: node('->', $1.ast, null, $3) }; }
  | postfix_expression INC
      { $$ = { type: $1.type, ast: node('++', $1.ast, null) }; }
  | postfix_expression DEC
      { $$ = { type: $1.type, ast: node('--', $1.ast, null) }; }
  ;



argument_expression_list_opt
  : argument_expression_list
      { $$ = { ast: { type: 'ArgList', children: $1 } }; }
  | /* vazio */
      { $$ = null; }
  ;

argument_expression_list
  : assignment_expression
      { $$ = [$1]; }
  | argument_expression_list ',' assignment_expression
      { $$ = $1.concat([$3]); }
  ;


/* ---------- PRIMÁRIOS ---------- */
primary_expression
  : ID        { const s = findSymbol($1); $$ = { type:s.type, ast: node('ID',null,null, $1) }; }
  | INT_LIT   { $$ = { type:'int',   ast: node('INT',null,null, yytext) }; }
  | F_LIT     { $$ = { type:'float', ast: node('FLOAT',null,null, yytext) }; }
  | CHAR_LIT  { $$ = { type:'char', ast: node('CHAR',null,null, yytext) }; }
  | STR_LIT   { $$ = { type:'char*', ast: node('STR',null,null, yytext) }; }
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
