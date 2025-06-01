/* >>> Gramática Léxica <<< */
/* Montar tabela de simbolos: com identificador, contador de escopo
    montar arvore para cada expressao (quebrar as expressoes em no maximo 3 variaveis)
    percorrendo a arvore da expressão em ordem é possivel ter o codigo intermediario
    fazer verificação de tipos: apenas gera o codigo se os tipos forem iguais, senao da um erro
*/

%{
    var escopoAtual = 0;
    var tabelaSimbolos = [];
    var tac = [];
    var erros = [];
    
    function criarVariavel(tipo, nome, valor, escopo){
        var flag = false;
        if(tabelaSimbolos.length == 0){
            tabelaSimbolos.push({ tipo: tipo, id: nome, val: valor, escopo: escopo });
            console.log('Primeira variavel criada');
        }
        else{
           ...
        }
    }
    
   
    
%}

%lex

%%

\s+                                 {console.log('BRANCO'); /* ignorar */}
"int"                               {console.log('Token INT'); return 'INT';}
"double"                            {console.log('Token DOUBLE'); return 'DOUBLE';}
"float"                             {console.log('Token FLOAT'); return 'FLOAT';}
"char"                              {console.log('Token CHAR'); return 'CHAR';}
"("                                 {console.log('Token (' ); return '(';}
")"                                 {console.log('Token )' ); return ')';}
"*"                                 {console.log('Token *'); return '*';}
"+"                                 {console.log('Token +'); return '+';}
"-"                                 {console.log('Token -'); return '-';}
"/"                                 {console.log('Token /'); return '/';}
";"                                 {console.log('Token ;'); return ';';}
":"                                 {console.log('Token :'); return ':';}
"."                                 {console.log('Token .'); return '.';}
","                                 {console.log('Token ,'); return ',';}
"'"                                 {console.log('Token QUOTE'); return 'QUOTE';}
'"'                                 {console.log('Token DQUOTE'); return 'DQUOTE';}
"["                                 {console.log('Token ['); return '[';}
"]"                                 {console.log('Token ]'); return ']';}
"{"                                 {console.log('Token {'); console.log(escopoAtual++); return '{';}
"}"                                 {console.log('Token }'); console.log(escopoAtual--); return '}';}
"<="                                {console.log('Token LE'); return 'LE';}
">="                                {console.log('Token GE'); return 'GE';}
"=="                                {console.log('Token EQ'); return 'EQ';}
"!="                                {console.log('Token NE'); return 'NE';}
"<"                                 {console.log('Token <'); return '<';}
">"                                 {console.log('Token >'); return '>';}
"="                                 {console.log('Token ='); return '=';}
"||"                                {console.log('Token OR'); return 'OR';}
"!"                                 {console.log('Token NOT'); return 'NOT';}
"&&"                                {console.log('Token AND'); return 'AND';}
"if"                                {console.log('Token IF'); return 'IF';}
"while"                             {console.log('Token WHILE'); return 'WHILE';}
"switch"                            {console.log('Token SWITCH'); return 'SWITCH';}
"case"                              {console.log('Token CASE'); return 'CASE';}
"break"                             {console.log('Token BREAK'); return 'BREAK';}
"default"                           {console.log('Token DEFAULT'); return 'DEFAULT';}
"else"                              {console.log('Token ELSE'); return 'ELSE';}
"var"                               {console.log('Token VAR'); return 'VAR';}
"do"                                {console.log('Token DO'); return 'DO';}
"#"                                 {console.log('Token #'); return '#';}
"define"                            {console.log('Token DEFINE'); return 'DEFINE';}
"for"                               {console.log('Token FOR'); return 'FOR';}
[a-zA-Z][a-zA-Z0-9_]*               {console.log('Token IDF', yytext); return 'IDF';}
[0-9]*\.[0-9]+([eE][+-][0-9]+)?     {console.log('Token F_LIT', yytext); return 'F_LIT';}
[0-9]+                              {console.log('Token INT_LIT', yytext); return 'INT_LIT';}
.                                   {console.log('Erro léxico: caractere [', yytext, '] não reconhecido.');}
<<EOF>>                             {return 'EOF';}

/lex
/* >>> Fim da Gramática Léxica */

/* >>> Start do Parser <<< */
%start expressions

/* permite usar simbolos como + e * na gramática, dizer que os simbolos podem aparecer
   zero, uma ou várias vezes (precisa ser colocado exatamente neste local do código) */
%ebnf

/* >>> Gramática BNF <<< */
%%


/* EXPRESSOES FINAIS */
expressions
    : AE* EOF
        %{  
            console.log('\n\nAnalálise sintática concluida com sucesso!');
            console.log('Análise Semântica');
            console.log('Tabela de simbolos:\n', tabelaSimbolos); 
            console.log('Códigos Three Address Code gerados\n', tac);
            console.log('Expressões contém algum erro semântico:\n', erros);
        %}
    ;
    
AE
    : var_exp
    | if_exp
    | else_exp
    | while_exp
    | for_exp
    | op
    | do_while_exp
    | define_exp
    | switch_exp
    ;
    
condicao
    : condicoes (comparacao condicoes)*
    ;
    

meio_comp
    :'>'
    |'<'
    |'GE'
    |'LE'
    |'NE'
    |'EQ'
    ;
    
comparacao
    : OR
    | AND
    | NOT
    ;
    
cond_vars
    :IDF
    |INT_LIT
    |FLOAT
    |CHAR
    |DOUBLE
    |F_LIT
    |ops_condicoes
    ;
    
condicoes
    : cond_vars meio_comp cond_vars
    ;
    
ops_condicoes
    : INT IDF
    | INT IDF '=' INT_LIT
    | FLOAT IDF
    | FLOAT IDF '=' F_LIT
    | DOUBLE IDF
    | DOUBLE IDF '=' F_LIT
    | CHAR IDF
    | CHAR IDF '=' QUOTE IDF QUOTE
    ;
 
/* CRIACAO DE VAIRAVEIS */
var_exp
    : INT IDF ';'
        {criarVariavel($1, $2, 0, escopoAtual); }
    | INT IDF '=' INT_LIT ';'
        { criarVariavel($1, $2, $4, escopoAtual); }
    | FLOAT IDF ';'
        { criarVariavel($1, $2, 0, escopoAtual); }
    ;
    
string
    : QUOTE IDF QUOTE
    ;
 
op_simbols
    : '+'
    | '-'
    | '*'
    | '/'
    ;

/* OPERACOES */
op
    : IDF '=' cond_vars ';'
    | IDF '=' cond_vars op_simbols cond_vars ';'
        { gerarCod($1, $3, $4, $5); }
    ;
    
op_condicao
    : IDF '=' cond_vars (op_simbols cond_vars)*
    ;
    
e
    : INT_LIT
    | DOUBLE
    | FLOAT
    | CHAR
    | '*'
    | '+'
    | '-'
    | '/'
    | ';'
    ;

/* >>> Fim da Gramática BNF <<< */
