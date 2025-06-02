/* =====================================================
 * compilador_c_completo.jison — Subconjunto ANSI‑C para Jison
 * Corrigido para pré-definir funções/constantes padrão C.
 * =====================================================*/

%{
/* ------------ CONTROLE DE ESCOPO & TIPO ------------ */
let currentScope = 0;            // 0 = global

// Inicializa a tabela de símbolos com funções e constantes padrão C
let symbolTable  = [
    // Funções de <stdio.h>
    { name: 'printf', type: 'function', scope: 0, returnType: 'int', params: [{type: 'char*'}] }, // Simplificado: aceita char* e varargs
    { name: 'scanf', type: 'function', scope: 0, returnType: 'int', params: [{type: 'char*'}] }, // Simplificado: aceita char* e varargs
    // Funções de <stdlib.h>
    { name: 'malloc', type: 'function', scope: 0, returnType: 'void*', params: [{type: 'int'}] }, // Simplificado: aceita size_t (int)
    { name: 'free', type: 'function', scope: 0, returnType: 'void', params: [{type: 'void*'}] },
    // Constantes
    { name: 'NULL', type: 'void*', scope: 0 } // NULL é geralmente (void*)0
    // Adicione outras funções/constantes padrão conforme necessário
];

let semanticErrors = [];''
let tac = [];                   // lista do código intermediário
let tempCount = 0;              // contador para temporários

function newTemp() { return `t${++tempCount}`; }

// Nó da árvore sintática
function node(op, left, right, lex) {
  return { op, left, right, lex };
}

// Geração do código de três endereços a partir da AST
function emitTAC(ast) {
  if (!ast) return null;

  // Se for um nó primário (ID, literal), retorna seu lexema/valor
  if (ast.op === 'ID' || ast.op === 'INT_LIT' || ast.op === 'F_LIT' || ast.op === 'CHAR_LIT' || ast.op === 'STR_LIT') {
      // Para IDs, usamos o nome diretamente. Para literais, o valor.
      return ast.lex !== undefined ? ast.lex : ast.value;
  }

  // Processa recursivamente os filhos
  const a = emitTAC(ast.left);
  const b = emitTAC(ast.right);

  // Cria um novo temporário para o resultado da operação
  const t = newTemp();
  
  // Monta a instrução TAC
  // Trata casos onde um operando pode não existir (ex: unário)
  if (b !== undefined && b !== null) {
      tac.push(`${t} = ${a} ${ast.op} ${b}`);
  } else if (a !== undefined && a !== null) {
      // Caso de operador unário (ex: t = -a)
      if (['-', '!', '~', '*'].includes(ast.op)) { // Adicionar outros unários se necessário
          tac.push(`${t} = ${ast.op}${a}`); 
      } else {
          // Outros casos unários ou erro?
          tac.push(`${t} = ${a}`); // Ou tratar especificamente
      }
  } else {
      // Caso sem operandos válidos? Pode ser um erro ou nó folha já tratado.
      // Retorna null ou trata como erro?
      return null; 
  }

  return t; // Retorna o nome do temporário que guarda o resultado
}

/* Escopo vem do léxico (enterScope/exitScope) */
function enterScope ()  { currentScope++; }
function exitScope  ()  {
  // Remove símbolos do escopo que está sendo fechado
  symbolTable = symbolTable.filter(s => s.scope < currentScope);
  currentScope--;
}

/* Função para registrar erros */
function reportError(msg) {
  const line = yy.lexer && yy.lexer.yylineno ? yy.lexer.yylineno : 'desconhecida';
  const errorMsg = `Linha ${line}: ${msg}`;
  // Evita duplicatas (opcional)
  if (!semanticErrors.includes(errorMsg)) {
      semanticErrors.push(errorMsg);
  }
}

/* ----------- tabela de símbolos ------------ */
function addSymbol(name, type) {
  // Verifica se já existe no escopo atual
  const existing = symbolTable.find(s => s.name === name && s.scope === currentScope);
  if (existing) {
      reportError(`Variável '${name}' redeclarada no escopo ${currentScope}.`);
      return false; // Falha ao adicionar
  }
  symbolTable.push({name, type, scope: currentScope});
  return true; // Sucesso
}

/* Busca variável visível mais interna */
function findSymbol(name) {
  for (let i = symbolTable.length - 1; i >= 0; --i) {
    const s = symbolTable[i];
    // Verifica se o símbolo está no escopo atual ou em um escopo pai
    if (s.name === name && s.scope <= currentScope) return s;
  }
  reportError(`Variável '${name}' não declarada.`);
  // Retorna símbolo "fantasma" para evitar crash e permitir continuar análise
  return { name, type: 'undefined', scope: -1 }; // Escopo -1 indica não encontrado
}

/* ----------- verificação de tipos ------------ */
// Função auxiliar para normalizar tipos (ex: char é int)
function normalizeType(type) {
    if (type === 'char') return 'int';
    // Adicionar outras normalizações se necessário (ex: short -> int)
    return type;
}

function typeEquals(t1, t2) {
  if (!t1 || !t2 || t1 === 'undefined' || t2 === 'undefined') return false;

  const nt1 = normalizeType(t1);
  const nt2 = normalizeType(t2);

  if (nt1 === nt2) return true;
  
  // Permitir operações entre tipos numéricos (int/float/double)
  const isNumeric = v => ['int', 'float', 'double'].includes(v);
  if (isNumeric(nt1) && isNumeric(nt2)) return true;

  // Permitir comparação de ponteiro com NULL (void*)
  if ((nt1.endsWith('*') && nt2 === 'void*') || (nt2.endsWith('*') && nt1 === 'void*')) return true;
  // Permitir comparação de ponteiro com int 0 (representação comum de NULL)
  if ((nt1.endsWith('*') && nt2 === 'int') || (nt2.endsWith('*') && nt1 === 'int')) {
      // Idealmente, verificar se o int é o literal 0
      return true; 
  }

  return false;
}

/* helper genérico para binários */
function checkBin(op, lhs, rhs) {
  if (!lhs || !rhs) {
    // reportError(`Operandos indefinidos na operação '${op}'`); // Erro já reportado por findSymbol
    return { type: 'undefined' };
  }
  const type1 = lhs.type;
  const type2 = rhs.type;

  if (!typeEquals(type1, type2)) {
    // Exceção: Atribuição permite conversão implícita (ex: int = float)
    if (op === '=') {
        const isNumeric = v => ['int', 'float', 'double', 'char'].includes(normalizeType(v));
        if (isNumeric(type1) && isNumeric(type2)) {
            // Permite atribuição entre numéricos, tipo resultante é o do L-value (lhs)
            return { type: type1 }; 
        }
        // Permitir atribuir NULL (void* ou int 0) a ponteiro
        if (type1.endsWith('*') && (type2 === 'void*' || type2 === 'int')) {
             // Idealmente verificar se o int é 0
             return { type: type1 };
        }
    }
    reportError(`Tipos incompatíveis em '${op}': ${type1} vs ${type2}`);
    return { type: 'undefined' };
  }

  // Determina o tipo do resultado
  const relationalOps = ['<','>','<=','>=','==','!='];
  const logicalOps = ['&&','||'];
  const bitwiseOps = ['&','|','^'];
  const shiftOps = ['<<','>>'];

  if (relationalOps.includes(op) || logicalOps.includes(op)) {
    return { type: 'int' }; // Resultado de comparações/lógicos é int (0 ou 1)
  }
  if (bitwiseOps.includes(op) || shiftOps.includes(op)) {
      if (normalizeType(type1) !== 'int' || normalizeType(type2) !== 'int') {
          reportError(`Operador '${op}' requer operandos inteiros.`);
          return { type: 'undefined' };
      }
      return { type: 'int' }; // Resultado de bitwise/shift é int
  }
  if (op === '%') {
      if (normalizeType(type1) !== 'int' || normalizeType(type2) !== 'int') {
          reportError(`Operador '%' requer operandos inteiros.`);
          return { type: 'undefined' };
      }
      return { type: 'int' };
  }
  
  // Para +, -, *, /: Preserva o tipo mais 'largo' (double > float > int)
  if (type1 === 'double' || type2 === 'double') return { type: 'double' };
  if (type1 === 'float' || type2 === 'float') return { type: 'float' };
  return { type: 'int' }; // Default para int/char
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
[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?    { yytext = parseFloat(yytext); return 'F_LIT'; }
[0-9]+                             { yytext = parseInt(yytext); return 'INT_LIT'; }
\'([^\\\']|\\.)\'                  { yytext = yytext.slice(1, -1); return 'CHAR_LIT'; } // Remove aspas
\"(\\.|[^"\\])*\"                  { yytext = yytext.slice(1, -1); return 'STR_LIT'; } // Remove aspas
[A-Za-z_][A-Za-z0-9_]*             return 'ID';

<<EOF>>        return 'EOF';
.              { console.error("Caractere desconhecido: " + yytext); return 'UNKNOWN'; }
/lex


/* ---------- 2. Tokens & Precedência ---------- */
%start program
%token AUTO BREAK CASE CHAR_T CONST CONTINUE DEFAULT DO DOUBLE_T ELSE ENUM EXTERN FLOAT_T FOR GOTO IF INT_T LONG_T REGISTER RETURN SHORT_T SIGNED SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE
%token DEFINE INCLUDE HEADER
%token INC DEC EQ NE LE GE AND OR LSHIFT RSHIFT LSHIFT_ASSIGN RSHIFT_ASSIGN PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN ARROW
%token INT_LIT F_LIT CHAR_LIT STR_LIT ID UNKNOWN LBRACE RBRACE

%left ','
%right '=' PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN LSHIFT_ASSIGN RSHIFT_ASSIGN
%right '?' ':'
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
%right '!' /* NOT lógico */
%right '~' /* NOT bitwise */
%right UMINUS /* Menos unário */
%right INC DEC /* Pré-incremento/decremento */
%right '&' '*' /* Endereço e Dereferência (unários) */
%right SIZEOF
/* Pós-incremento/decremento, chamada de função, acesso a membro/array têm maior precedência, tratados na gramática */

/* ---------- 3. Gramática ---------- */
%%
program
  : external_list EOF
      {
        console.log('\nTabela de Símbolos Final:');
        // Filtra símbolos internos ou não relevantes se necessário
        symbolTable.forEach(s => console.log(`${s.name} : ${s.type} : escopo ${s.scope}`));

        console.log('\nCódigo de Três Endereços:');
        tac.forEach(line => console.log(line));

        if (semanticErrors.length > 0) {
          console.log('\nErros Semânticos encontrados:');
          // Remove duplicatas antes de exibir
          [...new Set(semanticErrors)].forEach(e => console.log('- ' + e));
        } else {
          console.log('\nNenhum erro semântico encontrado.');
        }
        return { symbolTable, tac, errors: semanticErrors }; // Retorna resultado
      }
  ;

external_list
  : external_list external { $$ = $1; if ($2) $$.push($2); }
  | external { $$ = $1 ? [$1] : []; }
  ;

external
  : preprocessor_directive { $$ = null; } /* Ignora diretivas por enquanto */
  | function_definition { $$ = $1; }
  | declaration { $$ = $1; }
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

declaration
  : declaration_specifiers init_declarator_list_opt ';' { 
      // Ação principal está em init_declarator
      $$ = { type: 'declaration', specifiers: $1, declarators: $2 };
      __tipoDecl = null; // Limpa tipo global
    }
  ;

init_declarator_list_opt
  : init_declarator_list { $$ = $1; }
  | /* vazio */ { $$ = []; } // Permite declaração sem variável, ex: struct def;
  ;

function_definition
  : declaration_specifiers declarator compound_statement {
      const returnType = __tipoDecl; // Tipo base pego por declaration_specifiers
      const funcName = $2.id; // Nome da função vem do declarator
      // TODO: Adicionar função à tabela de símbolos (escopo pai)
      // TODO: Processar parâmetros ($2.params) e adicioná-los ao escopo da função
      $$ = { type: 'function_definition', name: funcName, returnType: returnType, params: $2.params, body: $3 };
      __tipoDecl = null;
    }
  ;

declaration_specifiers
  // Guarda o tipo base em __tipoDecl
  : type_specifier { $$ = $1; } // $1 já contém o tipo
  | storage_class_specifier { $$ = $1; }
  | type_qualifier { $$ = $1; }
  | declaration_specifiers type_specifier { $$ = $1; } // Ignora múltiplos tipos por enquanto
  | declaration_specifiers storage_class_specifier { $$ = $1; } // Ignora múltiplos storage
  | declaration_specifiers type_qualifier { $$ = $1; } // Ignora múltiplos qualifiers
  ;

type_qualifier
  : CONST { $$ = 'const'; }
  | VOLATILE { $$ = 'volatile'; }
  ;

storage_class_specifier
  : AUTO { $$ = 'auto'; }
  | REGISTER { $$ = 'register'; }
  | STATIC { $$ = 'static'; }
  | EXTERN { $$ = 'extern'; }
  | TYPEDEF { $$ = 'typedef'; }
  ;

type_specifier
  : CHAR_T      { __tipoDecl = 'char';   $$ = { type: 'char' }; }
  | INT_T       { __tipoDecl = 'int';    $$ = { type: 'int' }; }
  | FLOAT_T     { __tipoDecl = 'float';  $$ = { type: 'float' }; }
  | DOUBLE_T    { __tipoDecl = 'double'; $$ = { type: 'double' }; }
  | VOID        { __tipoDecl = 'void';   $$ = { type: 'void' }; }
  | struct_specifier { $$ = $1; } // Propaga tipo struct
  | union_specifier { $$ = $1; } // Propaga tipo union
  | enum_specifier { $$ = $1; } // Propaga tipo enum
  | SHORT_T     { __tipoDecl = 'short'; $$ = { type: 'short' }; } /* TODO: Combinar com int */
  | LONG_T      { __tipoDecl = 'long'; $$ = { type: 'long' }; } /* TODO: Combinar com int/double */
  | SIGNED      { __tipoDecl = 'signed'; $$ = { type: 'signed' }; } /* TODO: Combinar com int/char */
  | UNSIGNED    { __tipoDecl = 'unsigned'; $$ = { type: 'unsigned' }; } /* TODO: Combinar com int/char */
  ;

struct_specifier
  : STRUCT ID LBRACE struct_declaration_list RBRACE { /* TODO: Definir tipo struct */ $$ = { type: 'struct ' + $2, name: $2 }; __tipoDecl = 'struct ' + $2; }
  | STRUCT LBRACE struct_declaration_list RBRACE { /* TODO: Definir tipo struct anônima */ $$ = { type: 'struct', name: null }; __tipoDecl = 'struct'; }
  | STRUCT ID { /* TODO: Usar tipo struct existente */ $$ = { type: 'struct ' + $2, name: $2 }; __tipoDecl = 'struct ' + $2; }
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
  : init_declarator { $$ = [$1]; }
  | init_declarator_list ',' init_declarator { $$ = $1; $$.push($3); }
  ;

init_declarator
  : declarator { 
     | declarator { 
      const decl = $1;
      const finalType = (__tipoDecl || 'int') + (decl.pointer || '') + (decl.arrayInfo || ''); 
      if (addSymbol(decl.id, finalType)) {
          $$ = { id: decl.id, type: finalType };
      } else {
          $$ = null; // Erro na adição
      }
    }
  | declarator '=' initializer { 
      const declInfo = $1; // Renamed from decl to avoid conflict
      const initFinalType = (__tipoDecl || 'int') + (declInfo.pointer || '') + (declInfo.arrayInfo || ''); // Renamed finalType for this block
      if (addSymbol(declInfo.id, initFinalType)) {
          const lhs = { type: initFinalType, ast: node('ID', null, null, declInfo.id) };
          const rhs = $3; // initializer retorna { type, ast }
          const result = checkBin('=', lhs, rhs);
          result.ast = node('=', lhs.ast, rhs.ast);
          emitTAC(result.ast);
          $$ = { id: declInfo.id, type: initFinalType, initializer: rhs };
      } else {
          $$ = null;
      }
    }
  ;

declarator
  // Retorna { id: 'nome', pointer: '**', arrayInfo: '[][]' }
  : pointer_opt direct_declarator { 
      $$ = $2; // direct_declarator retorna { id, arrayInfo, params }
      $$.pointer = $1; // Adiciona info de ponteiro
    }
  ;

pointer_opt
  : '*' pointer_opt { $$ = '*' + ($2 || ''); }
  | /* vazio */ { $$ = ''; }
  ;

direct_declarator
  // Retorna { id, arrayInfo, params }
  : ID { $$ = { id: $1 }; }
  | '(' declarator ')' { $$ = $2; } // Propaga info interna
  | direct_declarator '[' constant_expression_opt ']' { 
      $$ = $1;
      $$.arrayInfo = ($$.arrayInfo || '') + '[]'; // Simplificado, não guarda tamanho
    }
  | direct_declarator '(' parameter_list_opt ')' { 
      $$ = $1;
      $$.isFunction = true;
      $$.params = $3; // Lista de parâmetros
    }
  ;

constant_expression_opt
  : constant_expression { $$ = $1; }
  | /* vazio */ { $$ = null; }
  ;

constant_expression
  : conditional_expression { $$ = $1; } // Simplificado: retorna a expressão
  ;

initializer
  : assignment_expression { $$ = $1; } // Retorna { type, ast }
  | LBRACE initializer_list RBRACE { $$ = $2; } // TODO: Tratar listas de inicialização
  ;

initializer_list
  : initializer { $$ = [$1]; }
  | initializer_list ',' initializer { $$ = $1; $$.push($3); }
  ;

parameter_list_opt
  : parameter_list { $$ = $1; }
  | /* vazio */ { $$ = []; }
  ;

parameter_list
  : parameter { $$ = [$1]; }
  | parameter_list ',' parameter { $$ = $1; $$.push($3); }
  ;

parameter
  // Retorna { name, type }
  : declaration_specifiers declarator { $$ = { name: $2.id, type: (__tipoDecl || 'int') + ($2.pointer || '') + ($2.arrayInfo || '') }; __tipoDecl = null; }
  | declaration_specifiers /* abstract declarator (ex: int func(int)) */ { $$ = { name: null, type: (__tipoDecl || 'int') }; __tipoDecl = null; }
  ;

/* ---------- Bloco e Instruções ---------- */

compound_statement
  : LBRACE block_item_list_opt RBRACE { $$ = { type: 'compound_statement', body: $2 }; }
  ;

block_item_list_opt
  : block_item_list { $$ = $1; }
  | /* vazio */ { $$ = []; }
  ;

block_item_list
  : block_item_list block_item { $$ = $1; if ($2) $$.push($2); }
  | block_item { $$ = $1 ? [$1] : []; }
  ;

block_item
  : declaration { $$ = $1; }
  | statement { $$ = $1; }
  ;

statement
  : labeled_statement { $$ = $1; }
  | compound_statement { $$ = $1; }
  | expression_statement { $$ = $1; }
  | selection_statement { $$ = $1; }
  | iteration_statement { $$ = $1; }
  | jump_statement { $$ = $1; }
  ;

labeled_statement
  : CASE constant_expression ':' statement { $$ = { type: 'case', value: $2, body: $4 }; }
  | DEFAULT ':' statement { $$ = { type: 'default', body: $3 }; }
  ;

expression_statement
  : expression_opt ';' { $$ = $1; } // Propaga o resultado da expressão (pode ser null)
  ;

expression_opt
  : expression { $$ = $1; } // Retorna { type, ast }
  | /* vazio */ { $$ = null; }
  ;

selection_statement
  : IF '(' expression ')' statement { /* TODO: Check expression type */ $$ = { type: 'if', cond: $3, body: $5 }; }
  | IF '(' expression ')' statement ELSE statement { /* TODO: Check expression type */ $$ = { type: 'if_else', cond: $3, then_body: $5, else_body: $7 }; }
  | SWITCH '(' expression ')' statement { /* TODO: Check expression type (int) */ $$ = { type: 'switch', expr: $3, body: $5 }; }
  ;

iteration_statement
  : WHILE '(' expression ')' statement { /* TODO: Check expression type */ $$ = { type: 'while', cond: $3, body: $5 }; }
  | DO statement WHILE '(' expression ')' ';' { /* TODO: Check expression type */ $$ = { type: 'do_while', body: $2, cond: $5 }; }
  | FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' statement { $$ = { type: 'for', init: $3, cond: $5, incr: $7, body: $9 }; }
  | FOR '(' declaration expression_opt ';' expression_opt ')' statement { /* TODO: Handle declaration scope */ $$ = { type: 'for_decl', decl: $3, cond: $4, incr: $6, body: $8 }; }
  ;

jump_statement
  : GOTO ID ';' { $$ = { type: 'goto', label: $2 }; }
  | CONTINUE ';' { $$ = { type: 'continue' }; }
  | BREAK ';' { $$ = { type: 'break' }; }
  | RETURN expression_opt ';' { /* TODO: Check return type against function type */ $$ = { type: 'return', value: $2 }; }
  ;

/* ------------- EXPRESSÕES ------------- */

expression
  : assignment_expression { $$ = $1; }
  | expression ',' assignment_expression { $$ = $3; } // Operador vírgula retorna o valor da direita
  ;

assignment_expression
  : conditional_expression { $$ = $1; }
  | unary_expression assignment_operator assignment_expression {
      const lhs = $1; // { type, ast }
      const op = $2;  // string do operador
      const rhs = $3; // { type, ast }
      // TODO: Verificar se lhs é L-value
      $$ = checkBin(op, lhs, rhs);
      $$.ast = node(op, lhs.ast, rhs.ast);
      emitTAC($$.ast);
    }
  ;

assignment_operator
  : '=' { $$ = '='; }
  | PLUS_ASSIGN { $$ = '+='; }
  | MINUS_ASSIGN { $$ = '-='; }
  | MUL_ASSIGN { $$ = '*='; }
  | DIV_ASSIGN { $$ = '/='; }
  | MOD_ASSIGN { $$ = '%='; }
  | AND_ASSIGN { $$ = '&='; }
  | OR_ASSIGN { $$ = '|='; }
  | XOR_ASSIGN { $$ = '^='; }
  | LSHIFT_ASSIGN { $$ = '<<='; }
  | RSHIFT_ASSIGN { $$ = '>>='; }
  ;

conditional_expression
  : logical_or_expression { $$ = $1; }
  | logical_or_expression '?' expression ':' conditional_expression {
      // TODO: Check condition type ($1)
      // TODO: Check compatibility between $3 and $5
      $$ = typeEquals($3.type, $5.type) ? $3 : { type: 'undefined' }; // Simplificado: assume tipo do 1º ramo se compatível
      $$.ast = node('?:', $1.ast, node(':', $3.ast, $5.ast)); // AST para ternário
    }
  ;

logical_or_expression
  : logical_and_expression { $$ = $1; }
  | logical_or_expression OR logical_and_expression {
      $$ = checkBin('||', $1, $3);
      $$.ast = node('||', $1.ast, $3.ast);
    }
  ;

logical_and_expression
  : inclusive_or_expression { $$ = $1; }
  | logical_and_expression AND inclusive_or_expression {
      $$ = checkBin('&&', $1, $3);
      $$.ast = node('&&', $1.ast, $3.ast);
    }
  ;

inclusive_or_expression
  : exclusive_or_expression { $$ = $1; }
  | inclusive_or_expression '|' exclusive_or_expression {
      $$ = checkBin('|', $1, $3);
      $$.ast = node('|', $1.ast, $3.ast);
    }
  ;

exclusive_or_expression
  : and_expression { $$ = $1; }
  | exclusive_or_expression '^' and_expression {
      $$ = checkBin('^', $1, $3);
      $$.ast = node('^', $1.ast, $3.ast);
    }
  ;

and_expression
  : equality_expression { $$ = $1; }
  | and_expression '&' equality_expression {
      $$ = checkBin('&', $1, $3);
      $$.ast = node('&', $1.ast, $3.ast);
    }
  ;

equality_expression
  : relational_expression { $$ = $1; }
  | equality_expression EQ relational_expression {
      $$ = checkBin('==', $1, $3);
      $$.ast = node('==', $1.ast, $3.ast);
    }
  | equality_expression NE relational_expression {
      $$ = checkBin('!=', $1, $3);
      $$.ast = node('!=', $1.ast, $3.ast);
    }
  ;

relational_expression
  : shift_expression { $$ = $1; }
  | relational_expression '<' shift_expression {
      $$ = checkBin('<', $1, $3);
      $$.ast = node('<', $1.ast, $3.ast);
    }
  | relational_expression '>' shift_expression {
      $$ = checkBin('>', $1, $3);
      $$.ast = node('>', $1.ast, $3.ast);
    }
  | relational_expression LE shift_expression {
      $$ = checkBin('<=', $1, $3);
      $$.ast = node('<=', $1.ast, $3.ast);
    }
  | relational_expression GE shift_expression {
      $$ = checkBin('>=', $1, $3);
      $$.ast = node('>=', $1.ast, $3.ast);
    }
  ;

shift_expression
  : additive_expression { $$ = $1; }
  | shift_expression LSHIFT additive_expression {
      $$ = checkBin('<<', $1, $3);
      $$.ast = node('<<', $1.ast, $3.ast);
    }
  | shift_expression RSHIFT additive_expression {
      $$ = checkBin('>>', $1, $3);
      $$.ast = node('>>', $1.ast, $3.ast);
    }
  ;

additive_expression
  : multiplicative_expression { $$ = $1; }
  | additive_expression '+' multiplicative_expression {
      $$ = checkBin('+', $1, $3);
      $$.ast = node('+', $1.ast, $3.ast);
    }
  | additive_expression '-' multiplicative_expression {
      $$ = checkBin('-', $1, $3);
      $$.ast = node('-', $1.ast, $3.ast);
    }
  ;

multiplicative_expression
  : cast_expression { $$ = $1; }
  | multiplicative_expression '*' cast_expression {
      $$ = checkBin('*', $1, $3);
      $$.ast = node('*', $1.ast, $3.ast);
    }
  | multiplicative_expression '/' cast_expression {
      $$ = checkBin('/', $1, $3);
      $$.ast = node('/', $1.ast, $3.ast);
    }
  | multiplicative_expression '%' cast_expression {
      $$ = checkBin('%', $1, $3);
      $$.ast = node('%', $1.ast, $3.ast);
    }
  ;

cast_expression
  : unary_expression { $$ = $1; }
  | '(' type_name ')' cast_expression { 
      // TODO: Implement cast type checking
      $$ = { type: $2.type, ast: node('cast', $4.ast, null, $2.type) }; // $2 é type_name
    }
  ;

unary_expression
  : postfix_expression { $$ = $1; }
  | INC unary_expression { /* TODO: Check L-value, type */ $$ = $2; $$.ast = node('pre_inc', $2.ast); }
  | DEC unary_expression { /* TODO: Check L-value, type */ $$ = $2; $$.ast = node('pre_dec', $2.ast); }
  | unary_operator cast_expression {
      const op = $1;
      const operand = $2; // { type, ast }
      let resultType = 'undefined';
      switch (op) {
          case '&': // Endereço de
              // TODO: Check if operand is L-value
              resultType = operand.type + '*'; // Simplificado
              break;
          case '*': // Dereferência
              if (operand.type.endsWith('*')) {
                  resultType = operand.type.slice(0, -1);
              } else {
                  reportError(`Não se pode dereferenciar tipo '${operand.type}'`);
              }
              break;
          case '+': // + Unário (geralmente no-op para tipos numéricos)
          case '-': // - Unário
              if (['int', 'float', 'double', 'char'].includes(normalizeType(operand.type))) {
                  resultType = operand.type;
              } else {
                  reportError(`Operador unário '${op}' não aplicável ao tipo '${operand.type}'`);
              }
              break;
          case '!': // NOT Lógico
              // TODO: Check if operand can be evaluated logically
              resultType = 'int';
              break;
          case '~': // NOT Bitwise
               if (normalizeType(operand.type) === 'int') {
                   resultType = 'int';
               } else {
                   reportError(`Operador '~' requer tipo inteiro.`);
               }
               break;
      }
      $$ = { type: resultType, ast: node(op, operand.ast) };
    }
  | SIZEOF unary_expression { $$ = { type: 'int', ast: node('sizeof_expr', $2.ast) }; } // size_t é int
  | SIZEOF '(' type_name ')' { $$ = { type: 'int', ast: node('sizeof_type', null, null, $3.type) }; }
  ;

unary_operator : '&' | '*' | '+' | '-' | '!' | '~' { $$ = $1; } ;

postfix_expression
  : primary_expression { $$ = $1; }
  | postfix_expression '[' expression ']' { 
      // TODO: Check if $1 is array/pointer, $3 is int
      const baseType = $1.type.replace(/(\[\]|\*)$/, ''); // Simplificado: remove '*' ou '[]'
      $$ = { type: baseType, ast: node('[]', $1.ast, $3.ast) }; // Tipo do elemento
    }
  | postfix_expression '(' argument_expression_list_opt ')' { 
      const func = $1; // { type, ast }
      const args = $3; // array de { type, ast }
      const symbol = findSymbol(func.ast.lex); // Busca a função na tabela
      let returnType = 'undefined';
      if (symbol.type === 'function') {
          returnType = symbol.returnType;
          // TODO: Check argument count and types against symbol.params
      } else if (func.type !== 'undefined') { // Permite chamar ponteiro de função?
          reportError(`Expressão '${func.ast.lex}' não é uma função.`);
      }
      $$ = { type: returnType, ast: node('call', func.ast, args.map(a => a.ast)) }; // AST para chamada
    }
  | postfix_expression '.' ID { 
      // TODO: Check if $1 is struct/union, $3 is member
      $$ = { type: 'undefined', ast: node('.', $1.ast, null, $3) }; // Tipo indefinido por enquanto
    }
  | postfix_expression ARROW ID { 
      // TODO: Check if $1 is pointer to struct/union, $3 is member
      $$ = { type: 'undefined', ast: node('->', $1.ast, null, $3) }; // Tipo indefinido por enquanto
    }
  | postfix_expression INC { /* TODO: Check L-value, type */ $$ = $1; $$.ast = node('post_inc', $1.ast); } // Valor é *antes* do incremento
  | postfix_expression DEC { /* TODO: Check L-value, type */ $$ = $1; $$.ast = node('post_dec', $1.ast); } // Valor é *antes* do decremento
  ;

argument_expression_list_opt
  : argument_expression_list { $$ = $1; }
  | /* vazio */ { $$ = []; }
  ;

argument_expression_list
  : assignment_expression { $$ = [$1]; } // Lista de { type, ast }
  | argument_expression_list ',' assignment_expression { $$ = $1; $$.push($3); }
  ;

primary_expression
  // Retorna { type, ast }
  : ID { 
      const symbol = findSymbol($1);
      $$ = { type: symbol.type, ast: node('ID', null, null, $1) }; 
    }
  | INT_LIT { $$ = { type: 'int', ast: node('INT_LIT', null, null, $1) }; }
  | F_LIT { $$ = { type: 'float', ast: node('F_LIT', null, null, $1) }; } // Ou double?
  | CHAR_LIT { $$ = { type: 'char', ast: node('CHAR_LIT', null, null, $1) }; }
  | STR_LIT { $$ = { type: 'char*', ast: node('STR_LIT', null, null, $1) }; } // String literal é char*
  | '(' expression ')' { $$ = $2; } // Propaga tipo e AST da expressão interna
  ;

type_name
  // Retorna { type, pointer }
  : type_specifier { $$ = { type: __tipoDecl, pointer: '' }; __tipoDecl = null; }
  | type_specifier pointer { $$ = { type: __tipoDecl, pointer: $2 }; __tipoDecl = null; }
  ;

pointer
  : '*' { $$ = '*'; }
  | '*' pointer { $$ = '*' + $2; }
  ;

union_specifier
  : UNION ID LBRACE struct_declaration_list RBRACE { /* TODO: Definir tipo union */ $$ = { type: 'union ' + $2, name: $2 }; __tipoDecl = 'union ' + $2; }
  | UNION LBRACE struct_declaration_list RBRACE { /* TODO: Definir tipo union anônima */ $$ = { type: 'union', name: null }; __tipoDecl = 'union'; }
  | UNION ID { /* TODO: Usar tipo union existente */ $$ = { type: 'union ' + $2, name: $2 }; __tipoDecl = 'union ' + $2; }
  ;

enum_specifier
  : ENUM ID LBRACE enumerator_list RBRACE { /* TODO: Definir tipo enum e constantes */ $$ = { type: 'enum ' + $2, name: $2 }; __tipoDecl = 'enum ' + $2; }
  | ENUM LBRACE enumerator_list RBRACE { /* TODO: Definir tipo enum anônima e constantes */ $$ = { type: 'enum', name: null }; __tipoDecl = 'enum'; }
  | ENUM ID { /* TODO: Usar tipo enum existente */ $$ = { type: 'enum ' + $2, name: $2 }; __tipoDecl = 'enum ' + $2; }
  ;

enumerator_list
  : enumerator
  | enumerator_list ',' enumerator
  ;

enumerator
  : ID { /* TODO: Add ID to symbol table as int constant */ }
  | ID '=' constant_expression { /* TODO: Add ID with value */ }
  ;

for_declaration
  : declaration_specifiers init_declarator_list /* Declaração dentro do for */
  ;

%%

