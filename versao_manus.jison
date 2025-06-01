/* =====================================================
 * compilador_c_completo_refatorado.jison — Subconjunto ANSI‑C para Jison
 * Refatorado para incluir verificação de escopo e tipo nas ações semânticas.
 * =====================================================*/

%{
  
  let scopeStack = [{}]; 
  let currentScopeLevel = 0;
  let errors = []; 
  let currentDeclarationType = null; 

  
  function reportError(message) {
    
    const line = yy.lexer && yy.lexer.yylineno ? yy.lexer.yylineno : 'desconhecida';
    const errorMsg = `Erro Semântico na Linha ${line}: ${message}`;
    errors.push(errorMsg);
    console.error(errorMsg);
  }

  
  function enterScope() {
    currentScopeLevel++;
    scopeStack.push({});
    
  }

  
  function exitScope() {
    if (currentScopeLevel > 0) {
      scopeStack.pop();
      currentScopeLevel--;
      
    } else {
      reportError("Tentativa de sair do escopo global.");
    }
  }

  
  function addVariable(name, type) {
    if (!type) {
        reportError(`Tipo não especificado para a variável '${name}'.`);
        return false;
    }
    
    const finalType = (typeof type === 'object' && type.type) ? type.type : type;
    if (typeof finalType !== 'string') {
        reportError(`Tipo inválido (${JSON.stringify(type)}) ao declarar variável '${name}'.`);
        return false;
    }

    const currentScope = scopeStack[scopeStack.length - 1];
    if (currentScope.hasOwnProperty(name)) {
      reportError(`Variável '${name}' redeclarada no escopo atual (nível ${currentScopeLevel}).`);
      return false;
    } else {
      currentScope[name] = { type: finalType, scope: currentScopeLevel };
      
      return true;
    }
  }

  
  function resolveVariable(name) {
    for (let i = scopeStack.length - 1; i >= 0; i--) {
      const scope = scopeStack[i];
      if (scope.hasOwnProperty(name)) {
        return scope[name]; 
      }
    }
    reportError(`Variável '${name}' não declarada.`);
    return null; 
  }

  
  function getExpressionType(exprNode) {
    if (!exprNode) {
        
        return null;
    }
    
    if (typeof exprNode === 'object' && exprNode.type) {
        return exprNode.type;
    }
    
    if (typeof exprNode === 'string') {
        const resolvedVar = resolveVariable(exprNode);
        if (resolvedVar) {
            return resolvedVar.type;
        }
        
        
        if (exprNode.match(/^[0-9]+$/)) return 'INT_T';
        if (exprNode.match(/^[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?$/)) return 'FLOAT_T'; 
        if (exprNode.match(/^'([^\\']|\\.)'$/)) return 'CHAR_T';
        if (exprNode.match(/^"(\\.|[^"\\])*"$/)) return 'STRING'; 
    }
    
    if (typeof exprNode === 'object' && exprNode.isLiteral) {
        return exprNode.type;
    }

    reportError(`Não foi possível determinar o tipo da expressão: ${JSON.stringify(exprNode)}`);
    return 'UNKNOWN'; 
  }

  
  function checkTypeCompatibility(op, type1, type2) {
    if (!type1 || !type2 || type1 === 'UNKNOWN' || type2 === 'UNKNOWN') {
        
        return false;
    }
    
    
    if (type1 !== type2) {
        reportError(`Incompatibilidade de tipos para operador '${op}': '${type1}' e '${type2}'.`);
        return false;
    }
    
    
    if (op === '%' && type1 !== 'INT_T') {
        reportError(`Operador '%' requer operandos inteiros, mas obteve '${type1}'.`);
        return false;
    }
    if ((op === '<<' || op === '>>') && (type1 !== 'INT_T' || type2 !== 'INT_T')) {
        reportError(`Operadores Shift ('${op}') requerem operandos inteiros.`);
        return false;
    }
    
    
    return true;
    
  function isLValue(exprNode) {
      
      return true;
      /*
      if (typeof exprNode === \'object\' && exprNode.isVar) {
          return true; 
      }
      
      reportError(`Expressão \'${JSON.stringify(exprNode)}\' não é um L-value válido para atribuição.`);
      return false;
      */
  }
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
.              { console.error("Caractere desconhecido: " + yytext); return 'UNKNOWN'; }
/lex


/* ---------- 2. Tokens & Precedência ---------- */
%start program
%token AUTO BREAK CASE CHAR_T CONST CONTINUE DEFAULT DO DOUBLE_T ELSE ENUM EXTERN FLOAT_T FOR GOTO IF INT_T LONG_T REGISTER RETURN SHORT_T SIGNED SIZEOF STATIC STRUCT SWITCH TYPEDEF UNION UNSIGNED VOID VOLATILE WHILE
%token DEFINE INCLUDE HEADER
%token INC DEC EQ NE LE GE AND OR LSHIFT RSHIFT LSHIFT_ASSIGN RSHIFT_ASSIGN PLUS_ASSIGN MINUS_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN ARROW
%token INT_LIT F_LIT CHAR_LIT STR_LIT ID UNKNOWN

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
%right '!'
%right UMINUS
%right INC DEC
%right '&' '*'
%right SIZEOF

/* ---------- 3. Gramática com Ações Semânticas ---------- */
%%
program
  : external_list EOF  {
                         if (errors.length > 0) {
                           console.log("\n--- Erros Semânticos Detectados ---");
                           errors.forEach(e => console.log(e));
                           
                           
                         } else {
                           console.log("\nAnálise semântica concluída sem erros.");
                         }
                         
                         return { ast: $1, errors: errors };
                       }
  ;

external_list
  : external_list external { $$ = $1; if ($2) $$.push($2); } /* Acumula nós válidos */
  | external { $$ = $1 ? [$1] : []; } /* Inicia a lista com o primeiro nó válido */
  ;

external
  : preprocessor_directive { $$ = $1; }
  | function_definition { $$ = $1; }
  | declaration { $$ = $1; }
  ;

preprocessor_directive
  : '#' INCLUDE HEADER { $$ = { type: 'include', file: $3 }; }
  | '#' DEFINE ID define_value_opt { $$ = { type: 'define', id: $3, value: $4 }; }
  ;

define_value_opt
  : expression { $$ = $1; }
  | STR_LIT { $$ = { type: 'STR_LIT', value: $1 }; }
  | /* vazio */ { $$ = null; }
  ;

declaration
  : declaration_specifiers init_declarator_list ';' { 
      $$ = { type: 'declaration', specifiers: $1, declarators: $2 }; 
      currentDeclarationType = null;
    }
  | declaration_specifiers ';' { 
      $$ = { type: 'type_declaration', specifiers: $1 }; 
      currentDeclarationType = null;
    }
  /* | ID init_declarator_list ';' 
  ;

function_definition
  : declaration_specifiers declarator compound_statement {
      const returnType = $1 ? $1.type : 'UNKNOWN';
      const funcName = $2 ? $2.id : 'UNKNOWN';
      $$ = { type: 'function_definition', returnType: returnType, name: funcName, parameters: $2.parameters, body: $3 };
      currentDeclarationType = null;
    }
  ;

declaration_specifiers
  
  : declaration_specifiers type_specifier { 
      let currentSpec = $1;
      currentSpec.type = $2.type; 
      currentSpec.specifiers.push($2); 
      $$ = currentSpec;
      currentDeclarationType = currentSpec.type; 
    }
  | declaration_specifiers type_qualifier { 
      let currentSpec = $1;
      currentSpec.specifiers.push($2); 
      $$ = currentSpec;
    }
  | declaration_specifiers storage_class_specifier { 
      let currentSpec = $1;
      currentSpec.specifiers.push($2); 
      $$ = currentSpec;
    }
  | type_specifier { 
      $$ = { type: $1.type, specifiers: [$1] }; 
      currentDeclarationType = $1.type;
    }
  | type_qualifier { $$ = { type: null, specifiers: [$1] }; } /* Qualificador sem tipo base ainda */
  | storage_class_specifier { $$ = { type: null, specifiers: [$1] }; } /* Storage class sem tipo base ainda */
  ;

type_qualifier
  : CONST { $$ = { qualifier: 'CONST' }; }
  | VOLATILE { $$ = { qualifier: 'VOLATILE' }; }
  ;

storage_class_specifier
  : AUTO { $$ = { storage: 'AUTO' }; }
  | REGISTER { $$ = { storage: 'REGISTER' }; }
  | STATIC { $$ = { storage: 'STATIC' }; }
  | EXTERN { $$ = { storage: 'EXTERN' }; }
  | TYPEDEF { $$ = { storage: 'TYPEDEF' }; }
  ;

type_specifier
  /* Retorna um objeto simples indicando o tipo base */
  : CHAR_T { $$ = { type: 'CHAR_T' }; }
  | INT_T { $$ = { type: 'INT_T' }; }
  | FLOAT_T { $$ = { type: 'FLOAT_T' }; }
  | DOUBLE_T { $$ = { type: 'DOUBLE_T' }; }
  | VOID { $$ = { type: 'VOID' }; }
  | struct_specifier { $$ = $1; } /* struct_specifier deve retornar { type: 'struct', ... } */
  | union_specifier { $$ = $1; } /* union_specifier deve retornar { type: 'union', ... } */
  | enum_specifier { $$ = $1; } /* enum_specifier deve retornar { type: 'enum', ... } */
  
  
  | SHORT_T { $$ = { type: 'SHORT_T' }; } /* Simplificado: tratado como tipo base */
  | LONG_T { $$ = { type: 'LONG_T' }; }   /* Simplificado */
  | SIGNED { $$ = { type: 'SIGNED' }; } /* Simplificado */
  | UNSIGNED { $$ = { type: 'UNSIGNED' }; } /* Simplificado */
  ;

struct_specifier
  
  : STRUCT ID '{' struct_declaration_list '}' { $$ = { type: 'struct', name: $2, members: $4 }; }
  | STRUCT '{' struct_declaration_list '}' { $$ = { type: 'struct', name: null, members: $3 }; } /* Anônima */
  | STRUCT ID { $$ = { type: 'struct_ref', name: $2 }; } /* Referência */
  ;

struct_declaration_list
  : struct_declaration { $$ = $1 ? [$1] : []; }
  | struct_declaration_list struct_declaration { $$ = $1; if ($2) $$.push($2); }
  ;

struct_declaration
  : declaration_specifiers struct_declarator_list ';' { $$ = { specifiers: $1, declarators: $2 }; }
  ;

struct_declarator_list
  : struct_declarator { $$ = $1 ? [$1] : []; }
  | struct_declarator_list ',' struct_declarator { $$ = $1; if ($3) $$.push($3); }
  ;

struct_declarator
  : declarator { $$ = $1; }
  ;

init_declarator_list
  : init_declarator { $$ = $1 ? [$1] : []; }
  | init_declarator_list ',' init_declarator { $$ = $1; if ($3) $$.push($3); }
  ;

init_declarator
  : declarator { 
      const declInfo = $1;
      if (declInfo && declInfo.id) {
          if (!addVariable(declInfo.id, currentDeclarationType)) {
              $$ = null; 
          } else {
              $$ = { declarator: declInfo, initializer: null }; 
          }
      } else {
          reportError("Declarador inválido sem ID.");
          $$ = null;
      }
    }
  | declarator '=' initializer {
      const declInfo = $1;
      let varAdded = false;
      if (declInfo && declInfo.id) {
          varAdded = addVariable(declInfo.id, currentDeclarationType);
      }
      
      if (!varAdded) {
          $$ = null; 
      } else {
          
          const initializerType = getExpressionType($3);
          const targetType = currentDeclarationType; 
          
          if (!checkTypeCompatibility('=', targetType, initializerType)) {
              
              
          }
          $$ = { declarator: declInfo, initializer: $3 }; 
      }
    }
  ;

declarator
  
  : pointer_opt direct_declarator { 
      $$ = $2; 
      if ($$) {
          $$.pointer = $1; 
      } else {
          reportError("Declarador direto inválido.");
          $$ = null;
      }
    }
  ;

pointer_opt
  : '*' pointer_opt { $$ = '*' + $2; } 
  | /* vazio */ { $$ = ''; }
  ;

direct_declarator
  : ID { $$ = { id: $1 }; }
  | '(' declarator ')' { $$ = $2; } 
  | direct_declarator '[' constant_expression_opt ']' { 
      $$ = $1;
      if ($$) {
          $$.isArray = true; 
          $$.arraySize = $3; 
      } else {
          reportError("Declarador base inválido para array.");
          $$ = null;
      }
    }
  | direct_declarator '(' parameter_list_opt ')' { 
      $$ = $1;
      if ($$) {
          $$.isFunction = true; 
          $$.parameters = $3; 
      } else {
          reportError("Declarador base inválido para função.");
          $$ = null;
      }
    }
  ;

parameter_list_opt
  : parameter_list { $$ = $1; }
  | /* vazio */ { $$ = []; }
  ;

parameter_list
  : parameter { $$ = $1 ? [$1] : []; }
  | parameter_list ',' parameter { $$ = $1; if ($3) $$.push($3); }
  ;

parameter
  
  : declaration_specifiers declarator { 
      $$ = { type: $1 ? $1.type : 'UNKNOWN', name: $2 ? $2.id : null, declarator: $2 }; 
    }
  | declaration_specifiers 
    { $$ = { type: $1 ? $1.type : 'UNKNOWN', name: null, declarator: null }; }
  ;

constant_expression_opt
  : constant_expression { $$ = $1; }
  | /* vazio */ { $$ = null; }
  ;

constant_expression
  
  : conditional_expression { $$ = $1; } 
  ;

initializer
  : assignment_expression { $$ = $1; } 
  | '{' initializer_list '}' { $$ = { type: 'initializer_list', values: $2 }; }
  | '{' initializer_list ',' '}' { $$ = { type: 'initializer_list', values: $2 }; } 
  ;

initializer_list
  : initializer { $$ = $1 ? [$1] : []; }
  | initializer_list ',' initializer { $$ = $1; if ($3) $$.push($3); }
  ;

/* ---------- Bloco e Instruções ---------- */

compound_statement
  
  : '{' { enterScope(); } block_item_list_opt '}' { exitScope(); $$ = { type: 'compound_statement', items: $3 }; }
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
  : CASE constant_expression ':' statement { $$ = { type: 'case_label', value: $2, statement: $4 }; }
  | DEFAULT ':' statement { $$ = { type: 'default_label', statement: $3 }; }
  ;

expression_statement
  : expression_opt ';' { $$ = { type: 'expression_statement', expression: $1 }; }
  ;

expression_opt
  : expression { $$ = $1; }
  | /* vazio */ { $$ = null; }
  ;

selection_statement
  : IF '(' expression ')' statement { 
      
      $$ = { type: 'if', condition: $3, then_branch: $5 }; 
    }
  | IF '(' expression ')' statement ELSE statement { 
      $$ = { type: 'if_else', condition: $3, then_branch: $5, else_branch: $7 }; 
    }
  | SWITCH '(' expression ')' statement { 
      
      $$ = { type: 'switch', expression: $3, body: $5 }; 
    }
  ;

iteration_statement
  : WHILE '(' expression ')' statement { 
      $$ = { type: 'while', condition: $3, body: $5 }; 
    }
  | DO statement WHILE '(' expression ')' ';' { 
      $$ = { type: 'do_while', body: $2, condition: $5 }; 
    }
  | FOR '(' expression_opt ';' expression_opt ';' expression_opt ')' statement { 
      $$ = { type: 'for', init: $3, condition: $5, increment: $7, body: $9 }; 
    }
  | FOR '(' for_declaration expression_opt ';' expression_opt ')' statement { 
      
      $$ = { type: 'for_decl', declaration: $3, condition: $4, increment: $6, body: $8 }; 
    }
  ;

jump_statement
  : GOTO ID ';' { $$ = { type: 'goto', label: $2 }; }
  | CONTINUE ';' { $$ = { type: 'continue' }; }
  | BREAK ';' { $$ = { type: 'break' }; }
  | RETURN expression_opt ';' { 
      
      $$ = { type: 'return', expression: $2 }; 
    }
  ;

/* ------------- Expressões com Verificação de Tipo e Escopo ------------- */
expression
  : assignment_expression { $$ = $1; }
  | expression ',' assignment_expression { 
      
      $$ = $3; 
    }
  ;

assignment_expression
  : conditional_expression { $$ = $1; }
  | unary_expression assignment_operator assignment_expression {
      const lvalue = $1;
      const rvalue = $3;
      const op = $2; 

      
      if (!isLValue(lvalue)) {
          $$ = { type: 'UNKNOWN', error: 'L-value required' }; 
      } else {
          
          const lvalueType = getExpressionType(lvalue);
          const rvalueType = getExpressionType(rvalue);
          
          
          if (!checkTypeCompatibility(op, lvalueType, rvalueType)) {
              
              $$ = { type: 'UNKNOWN', error: 'Type mismatch' };
          } else {
              
              $$ = { type: lvalueType, value: null, op: op, left: lvalue, right: rvalue }; 
          }
      }
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
      const condType = getExpressionType($1);
      const trueType = getExpressionType($3);
      const falseType = getExpressionType($5);
      
      
      if (!checkTypeCompatibility('?:', trueType, falseType)) {
          
          $$ = { type: 'UNKNOWN', error: 'Type mismatch in ternary op' };
      } else {
          $$ = { type: trueType, value: null, op: '?:', cond: $1, trueExpr: $3, falseExpr: $5 };
      }
    }
  ;


logical_or_expression
  : logical_and_expression { $$ = $1; }
  | logical_or_expression OR logical_and_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      
      
      $$ = { type: 'INT_T', value: null, op: '||', left: $1, right: $3 }; 
    }
  ;

logical_and_expression
  : inclusive_or_expression { $$ = $1; }
  | logical_and_expression AND inclusive_or_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      
      $$ = { type: 'INT_T', value: null, op: '&&', left: $1, right: $3 };
    }
  ;


inclusive_or_expression
  : exclusive_or_expression { $$ = $1; }
  | inclusive_or_expression '|' exclusive_or_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      
      if (type1 !== 'INT_T' || type2 !== 'INT_T') { reportError("Operador '|' requer operandos inteiros."); }
      if (!checkTypeCompatibility('|', type1, type2)) { 
          $$ = { type: 'UNKNOWN', error: 'Type mismatch' };
      } else {
          $$ = { type: type1, value: null, op: '|', left: $1, right: $3 }; 
      }
    }
  ;

exclusive_or_expression
  : and_expression { $$ = $1; }
  | exclusive_or_expression '^' and_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (type1 !== 'INT_T' || type2 !== 'INT_T') { reportError("Operador '^' requer operandos inteiros."); }
      if (!checkTypeCompatibility('^', type1, type2)) {
          $$ = { type: 'UNKNOWN', error: 'Type mismatch' };
      } else {
          $$ = { type: type1, value: null, op: '^', left: $1, right: $3 };
      }
    }
  ;

and_expression
  : equality_expression { $$ = $1; }
  | and_expression '&' equality_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (type1 !== 'INT_T' || type2 !== 'INT_T') { reportError("Operador '&' requer operandos inteiros."); }
      if (!checkTypeCompatibility('&', type1, type2)) {
          $$ = { type: 'UNKNOWN', error: 'Type mismatch' };
      } else {
          $$ = { type: type1, value: null, op: '&', left: $1, right: $3 };
      }
    }
  ;


equality_expression
  : relational_expression { $$ = $1; }
  | equality_expression EQ relational_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('==', type1, type2)) {
          $$ = { type: 'UNKNOWN', error: 'Type mismatch' };
      } else {
          $$ = { type: 'INT_T', value: null, op: '==', left: $1, right: $3 }; 
      }
    }
  | equality_expression NE relational_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('!=', type1, type2)) {
          $$ = { type: 'UNKNOWN', error: 'Type mismatch' };
      } else {
          $$ = { type: 'INT_T', value: null, op: '!=', left: $1, right: $3 };
      }
    }
  ;


relational_expression
  : shift_expression { $$ = $1; }
  | relational_expression '<' shift_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('<', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: 'INT_T', value: null, op: '<', left: $1, right: $3 }; }
    }
  | relational_expression '>' shift_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('>', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: 'INT_T', value: null, op: '>', left: $1, right: $3 }; }
    }
  | relational_expression LE shift_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('<=', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: 'INT_T', value: null, op: '<=', left: $1, right: $3 }; }
    }
  | relational_expression GE shift_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('>=', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: 'INT_T', value: null, op: '>=', left: $1, right: $3 }; }
    }
  ;


shift_expression
  : additive_expression { $$ = $1; }
  | shift_expression LSHIFT additive_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      
      if (type1 !== 'INT_T' || type2 !== 'INT_T') { reportError("Operador '<<' requer operandos inteiros."); }
      if (!checkTypeCompatibility('<<', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: 'INT_T', value: null, op: '<<', left: $1, right: $3 }; }
    }
  | shift_expression RSHIFT additive_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (type1 !== 'INT_T' || type2 !== 'INT_T') { reportError("Operador '>>' requer operandos inteiros."); }
      if (!checkTypeCompatibility('>>', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: 'INT_T', value: null, op: '>>', left: $1, right: $3 }; }
    }
  ;

additive_expression
  : multiplicative_expression { $$ = $1; }
  | additive_expression '+' multiplicative_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('+', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: type1, value: null, op: '+', left: $1, right: $3 }; }
    }
  | additive_expression '-' multiplicative_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('-', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: type1, value: null, op: '-', left: $1, right: $3 }; }
    }
  ;

multiplicative_expression
  : cast_expression { $$ = $1; }
  | multiplicative_expression '*' cast_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('*', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: type1, value: null, op: '*', left: $1, right: $3 }; }
    }
  | multiplicative_expression '/' cast_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (!checkTypeCompatibility('/', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: type1, value: null, op: '/', left: $1, right: $3 }; }
    }
  | multiplicative_expression '%' cast_expression {
      const type1 = getExpressionType($1);
      const type2 = getExpressionType($3);
      if (type1 !== 'INT_T' || type2 !== 'INT_T') { reportError("Operador '%' requer operandos inteiros."); }
      if (!checkTypeCompatibility('%', type1, type2)) { $$ = { type: 'UNKNOWN', error: 'Type mismatch' }; }
      else { $$ = { type: 'INT_T', value: null, op: '%', left: $1, right: $3 }; }
    }
  ;

cast_expression
  : unary_expression { $$ = $1; }
  | '(' type_name ')' cast_expression {
      const targetType = $2 ? $2.type : 'UNKNOWN';
      const exprType = getExpressionType($4);

      $$ = { type: targetType, value: null, op: 'cast', target: $2, expression: $4 };
    }
  ;

unary_expression
  : postfix_expression { $$ = $1; }
  | INC unary_expression { 
      const expr = $2;
      const exprType = getExpressionType(expr);
      if (exprType !== 'INT_T' && exprType !== 'FLOAT_T' && exprType !== 'DOUBLE_T') { reportError("Operador pré '++' requer operando numérico."); }
      if (!isLValue(expr)) { reportError("Operador pré '++' requer um L-value."); }
      $$ = { type: exprType, value: null, op: 'pre_inc', operand: expr }; 
    }
  | DEC unary_expression { 
      const expr = $2;
      const exprType = getExpressionType(expr);
      if (exprType !== 'INT_T' && exprType !== 'FLOAT_T' && exprType !== 'DOUBLE_T') { reportError("Operador pré '--' requer operando numérico."); }
      if (!isLValue(expr)) { reportError("Operador pré '--' requer um L-value."); }
      $$ = { type: exprType, value: null, op: 'pre_dec', operand: expr }; 
    }
  | unary_operator cast_expression { 
      const op = $1;
      const expr = $2;
      const exprType = getExpressionType(expr);
      let resultType = 'UNKNOWN';
      switch (op) {
          case '&': 
              if (!isLValue(expr)) { reportError("Operador '&' requer um L-value."); }
              resultType = exprType + '*';
              break;
          case '*':
              if (typeof exprType === 'string' && exprType.endsWith('*')) {
                  resultType = exprType.slice(0, -1);
              } else {
                  reportError(`Operador '*' requer um operando ponteiro, obteve '${exprType}'.`);
              }
              break;
          case '+':
          case '-':
              if (exprType !== 'INT_T' && exprType !== 'FLOAT_T' && exprType !== 'DOUBLE_T') { reportError(`Operador unário '${op}' requer operando numérico.`); }
              resultType = exprType;
              break;
          case '!':
              resultType = 'INT_T';
              break;
      }
      $$ = { type: resultType, value: null, op: op, operand: expr };
    }
  | SIZEOF unary_expression { $$ = { type: 'INT_T', value: null, op: 'sizeof_expr', operand: $2 }; }
  | SIZEOF '(' type_name ')' { $$ = { type: 'INT_T', value: null, op: 'sizeof_type', target: $3 }; }
  ;

unary_operator
  : '&' { $$ = '&'; }
  | '*' { $$ = '*'; }
  | '+' { $$ = '+'; }
  | '-' { $$ = '-'; }
  | '!' { $$ = '!'; }
  ;

postfix_expression
  : primary_expression { $$ = $1; }
  | postfix_expression '[' expression ']' { 
      const arrayExpr = $1;
      const indexExpr = $3;
      const arrayType = getExpressionType(arrayExpr);
      const indexType = getExpressionType(indexExpr);
      let elementType = 'UNKNOWN';
      if (indexType !== 'INT_T') { reportError("Índice do array deve ser inteiro."); }
    
      $$ = { type: elementType, value: null, op: '[]', array: arrayExpr, index: indexExpr, isLValue: true };
    }
  | postfix_expression '(' argument_expression_list_opt ')' { 
      const funcExpr = $1;
      const args = $3;
      const funcName = funcExpr.value;
      const funcInfo = resolveVariable(funcName);
      let returnType = 'UNKNOWN';
      $$ = { type: returnType, value: null, op: '()', function: funcExpr, arguments: args };
    }
  | postfix_expression '.' ID { 
      const structExpr = $1;
      const memberName = $3;
      const structType = getExpressionType(structExpr);
      let memberType = 'UNKNOWN';
      $$ = { type: memberType, value: null, op: '.', struct: structExpr, member: memberName, isLValue: true };
    }
  | postfix_expression ARROW ID { 
      const pointerExpr = $1;
      const memberName = $3;
      const pointerType = getExpressionType(pointerExpr);
      let memberType = 'UNKNOWN';
      $$ = { type: memberType, value: null, op: '->', pointer: pointerExpr, member: memberName, isLValue: true };
    }
  | postfix_expression INC { 
      const expr = $1;
      const exprType = getExpressionType(expr);
      if (exprType !== 'INT_T' && exprType !== 'FLOAT_T' && exprType !== 'DOUBLE_T') { reportError("Operador pós '++' requer operando numérico."); }
      if (!isLValue(expr)) { reportError("Operador pós '++' requer um L-value."); }
      $$ = { type: exprType, value: null, op: 'post_inc', operand: expr };
    }
  | postfix_expression DEC { 
      const expr = $1;
      const exprType = getExpressionType(expr);
      if (exprType !== 'INT_T' && exprType !== 'FLOAT_T' && exprType !== 'DOUBLE_T') { reportError("Operador pós '--' requer operando numérico."); }
      if (!isLValue(expr)) { reportError("Operador pós '--' requer um L-value."); }
      $$ = { type: exprType, value: null, op: 'post_dec', operand: expr }; 
    }
  ;

argument_expression_list_opt
  : argument_expression_list { $$ = $1; }
  | /* vazio */ { $$ = []; }
  ;

argument_expression_list
  : assignment_expression { $$ = $1 ? [$1] : []; }
  | argument_expression_list ',' assignment_expression { $$ = $1; if ($3) $$.push($3); }
  ;

primary_expression
  : ID { 
      const resolved = resolveVariable($1);
      if (resolved) {
          $$ = { type: resolved.type, value: $1, isVar: true, scope: resolved.scope };
      } else {
          $$ = { type: 'UNKNOWN', value: $1, error: 'Undeclared variable' };
      }
    }
  | INT_LIT { $$ = { type: 'INT_T', value: parseInt($1), isLiteral: true }; }
  | F_LIT { $$ = { type: 'FLOAT_T', value: parseFloat($1), isLiteral: true }; }
  | CHAR_LIT { $$ = { type: 'CHAR_T', value: $1, isLiteral: true }; } 
  | STR_LIT { $$ = { type: 'STRING', value: $1, isLiteral: true }; } 
  | '(' expression ')' { $$ = $2; } 
  ;

type_name
  : type_specifier { $$ = { type: $1.type, pointer: '' }; }
  | type_specifier pointer { $$ = { type: $1.type, pointer: $2 }; }
  ;

pointer
  : '*' { $$ = '*'; }
  | '*' pointer { $$ = '*' + $2; }
  ;

union_specifier
  : UNION ID '{' struct_declaration_list '}' { $$ = { type: 'union', name: $2, members: $4 }; }
  | UNION '{' struct_declaration_list '}' { $$ = { type: 'union', name: null, members: $3 }; }
  | UNION ID { $$ = { type: 'union_ref', name: $2 }; }
  ;

enum_specifier
  : ENUM ID '{' enumerator_list '}' { $$ = { type: 'enum', name: $2, enumerators: $4 }; }
  | ENUM '{' enumerator_list '}' { $$ = { type: 'enum', name: null, enumerators: $3 }; }
  | ENUM ID { $$ = { type: 'enum_ref', name: $2 }; }
  ;

enumerator_list
  : enumerator { $$ = $1 ? [$1] : []; }
  | enumerator_list ',' enumerator { $$ = $1; if ($3) $$.push($3); }
  ;

enumerator
  : ID { 
      if (!addVariable($1, 'INT_T')) { $$ = null; }
      else { $$ = { id: $1, value: null }; }
    }
  | ID '=' constant_expression { 
      if (!addVariable($1, 'INT_T')) { $$ = null; }
      else { $$ = { id: $1, value: $3 }; }
    }
  ;

for_declaration
  : declaration_specifiers init_declarator_list { 
      $$ = { type: 'for_declaration', specifiers: $1, declarators: $2 }; 
      currentDeclarationType = null;
    }
  ;

%%

