grammar AntlrGrinder;

expression : expr ;

expr : 
       // parenthesis, e.g.:(1+2)
     '(' expr ')' #parenthesesAroundExpression
       // an expression symbol, e.g.:<X + Y>
     | '<' expr '>' #expressionSymbol
       // function application, e.g.: f(X)
     | expr_function_application # functionApplication
       // tuple, e.g.: (A, B, C)
     | '(' expr ',' expr (',' expr)* ')' #tuple
       // counting formula, e.g.: | X in 1..10 : X < 5 |
     | '|' ( indexes+=expr (',' indexes+=expr)* )? ':' body=expr '|' #countingFormula
       // cardinality, e.g.: | X |
     | '|' expr '|' #cardinality
       // intensional uniset, e.g.: { (on X) f(X) | X != a }
     | '{' ('(' scope=ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr (':' condition=expr)? '}' #intensionalUniset
       // intensional multiset, e.g.: {{ (on X) f(X) | X != a }}
     | '{{' ('(' scope=ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr (':' condition=expr)? '}}' #intensionalMultiset
       // extensional uniset, e.g.: { A, B, C, C, D }
     | '{' ( expr (',' expr)* )? '}' #extensionalUniset
       // extensional multiset, e.g.: {{ A, B, C, C, D }}
     | '{{' ( expr (',' expr)* )? '}}' #extensionalMultiset
       // bracketed expression, for parfactors and random variables, e.g. [if p(X) then 1 else 2]
     | '[' expr ']' #bracketedExpression
       // not, e.g.: not A and B -> (not(A)) and B
     | NOT expr #not
       // negative, e.g.: 2 * -1 -> 2 * (-1)
     | '-' expr #negative // We set the unary minus to higher precedence
       // NOTE:  P)arentheses, E)xponents, ( M)ultiplication, D)ivision and special case of Integer Interval '..' ), ( A)ddition, S)ubtraction )
       // see: http://en.wikipedia.org/wiki/Order_of_operations
       // exponentiation, e.g. 2^3^4 -> 2^(3^4)
     |<assoc=right> base=expr '^' exponent=expr #Exponentiation
       // multiplication or division or an Integer Interval, e.g.: 2*3/2 -> 2*(3/2)
     | leftop=expr op=('*' | '/' | '..') rightop=expr #multiplicationOrDivisionOrIntegerInterval
       // addition or subtraction, e.g.: 1-2+3 -> (1-2)+3
     | leftop=expr op=('+' | '-') rightop=expr #additionOrSubtraction
       // real interval
     | leftBracket=('[' | ']') lower=expr SEMICOLON upper=expr rightBracket=(']' | '[') #realInterval
       // set intersection, e.g.: {a, b, c} intersection {b}
     | leftop=expr INTERSECTION rightop=expr #intersection
       // set union, {a, b, c} union {b, d}
     | leftop=expr UNION rightop=expr #union
       // set membership, x in {x, y, z}
     | leftop=expr IN rightop=expr #in
       // comparison operators, e.g.: X = Y, 2 < 3
     | leftop=expr op=('<' | '<=' | '=' | '!=' | '>=' | '>') rightop=expr #comparison
       // conjunction, e.g.: A or B and C -> A or (B and C)
     | leftconj=expr AND rightconj=expr #and
       // disjunction, e.g.: A => B or C -> A => (B or C)
     | leftdisj=expr OR rightdisj=expr #or
       // implication, e.g.: A = B => C = D
     |<assoc=right> antecedent=expr IMPLICATION consequent=expr #implication
       // biconditional, e.g.: A = B <=> C = D
     |<assoc=right> leftop=expr BICONDITIONAL rightop=expr #biconditional
       // conditional, e.g.: if X = Y then 1 else 2
     | IF condition=expr THEN thenbranch=expr ELSE elsebranch=expr #ifThenElse
       // lambda, e.g.: lambda f(X) : 2 + f(X)
     | expr_lambda #lamda
       // universal quantification, e.g.: for all X : X != a
     | FOR ALL index=expr ':' body=expr #forAll
       // existential quantification, e.g.: there exists X : X = a
     | THERE EXISTS index=expr ':' body=expr #thereExists
       // cartesian product
     | firstarg=expr_constant_name (X additionalargs+=expr_constant_name)+ #cartesianProduct
       // function type
     | domaintypes+=expr_constant_name (X domaintypes+=expr_constant_name)* FUNCTION_TYPE rangetype=expr_constant_name #functionType
     | expr_symbol #symbol
     ;
     
expr_function_application
     : functor=expr_functor '(' ( args+=expr (',' args+=expr)* )? ')'
     ;
     
expr_functor
    : expr_symbol          #functorSymbol
    | '(' lambda=expr_lambda ')'  #functorLambda
    | functor=expr_symbol '(' ( args+=expr (',' args+=expr)* )? ')' #functorFunctionApplication
    ;    
    
expr_lambda
    : LAMBDA ( parameters+=expr (',' parameters+=expr)* )? ':' body=expr
    ; 
     
expr_symbol
    : expr_constant_name
    | expr_constant_number
      // NOTE: even though the expr_constant_name pattern should match these keywords
      // ANTLR excludes the set of keywords/tokens that are used from matching
      // this pattern (see chapter5 of reference manual). Therefore, we
      // need to explicitly list all of the keywords and operators that we
      // want also to be interpreted as Symbols.
    | NOT | AND | OR | FOR | ALL | THERE | EXISTS // Keywords
    | LAMBDA | IF | THEN | ELSE
    | INTERSECTION | UNION
    | ON | IN
    | X | FUNCTION_TYPE
    | IMPLICATION | BICONDITIONAL // Logic Operators
    | EXPONENTIATION | DIVIDE | TIMES | PLUS | SUBTRACT // Arithmetic
    | LESS_THAN_EQUAL | EQUAL | NOT_EQUAL | GREATER_THAN_EQUAL // Comparison
    | COLON | VERT_BAR | UNDERSCORE | PERIOD // Misc    
    ;
    
expr_constant_name
    : CONSTANT_STR
    | QUOTED_CONSTANT_STR
    ;
    
expr_constant_number
    : INTEGER
    | RATIONAL
    ;

/*
    The lexer tokenizes the input string that the parser is asked to
    parse.  The tokens are all typed. Whitespace
    symbols will be excluded from the resulting token stream.

    Adding new grammar rules
    ------------------------
    Add any terminal symbols in the new grammar rules to the list below.

    Note: Ensure you update the corresponding list in AntlrGrinderTerminalSymbols.java
          with any changes made.
    
*/
// Keywords
NOT                     : 'not' ;
AND                     : 'and' ;
OR                      : 'or' ;
FOR                     : 'for' ;
ALL                     : 'all' ;
THERE                   : 'there' ;
EXISTS                  : 'exists' ;
LAMBDA                  : 'lambda' ;
IF                      : 'if' ;
THEN                    : 'then' ;
ELSE                    : 'else' ;
INTERSECTION            : 'intersection' ;
UNION                   : 'union' ;
ON                      : 'on' ;
IN                      : 'in' ;
// Special Functions
X                       : 'x' ; // Used for Cartesian Product
FUNCTION_TYPE           : '->' ;
// Logic Operators
IMPLICATION             : '=>' ;
BICONDITIONAL           : '<=>' ;
// Arithmetic
EXPONENTIATION          : '^' ;
DIVIDE                  : '/' ;
TIMES                   : '*' ;
INTEGER_INTERVAL        : '..' ;
PLUS                    : '+' ;
SUBTRACT                : '-' ;
// Comparison
LESS_THAN               : '<' ;
LESS_THAN_EQUAL         : '<=' ;
EQUAL                   : '=' ;
NOT_EQUAL               : '!=' ;
GREATER_THAN_EQUAL      : '>=' ;
GREATER_THAN            : '>' ;
// Brackets
OPEN_PAREN              : '(' ;
CLOSE_PAREN             : ')' ;
OPEN_SQUARE             : '[' ;
CLOSE_SQUARE            : ']' ;
OPEN_CURLY              : '{' ;
CLOSE_CURLY             : '}' ;
OPEN_DOUBLE_CURLY       : '{{' ;
CLOSE_DOUBLE_CURLY      : '}}' ;
// Misc
SEMICOLON               : ';' ;
COLON                   : ':' ;
VERT_BAR                : '|' ;
COMMA                   : ',' ;
UNDERSCORE              : '_' ;
PERIOD                  : '.' ;

INTEGER 
    : ('0'..'9')+
    ;
    
RATIONAL
    : ('0' | '1'..'9' '0'..'9'*)
    | ('0'..'9')+ '.' ('0'..'9')+ EXPONENT? FLOAT_TYPE_SUFFIX?
    | '.' ('0'..'9')+ EXPONENT? FLOAT_TYPE_SUFFIX?
    | ('0'..'9')+ EXPONENT FLOAT_TYPE_SUFFIX?
    | ('0'..'9')+ FLOAT_TYPE_SUFFIX
    | ('0x' | '0X') (HEX_DIGIT )*
      ('.' (HEX_DIGIT)*)?
      ( 'p' | 'P' )
      ( '+' | '-' )?
      ( '0' .. '9' )+
      FLOAT_TYPE_SUFFIX?
    ;

CONSTANT_STR 
    : ([a-zA-Z] | [0-9] | '_') ([a-zA-Z] | [0-9] | '_')* ('\'')*
    ;

QUOTED_CONSTANT_STR
    : '"'  (ESCAPE_SEQUENCE | ~('\\' | '"' ) )* '"'
    | '\'' (ESCAPE_SEQUENCE | ~('\\' | '\'') )* '\''
    ;

fragment
EXPONENT : ('e'|'E') ('+'|'-')? ('0'..'9')+
         ;

fragment
FLOAT_TYPE_SUFFIX : ('f'|'F'|'d'|'D')
                  ;

fragment
ESCAPE_SEQUENCE
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\')
    |   UNICODE_ESCAPE
    |   OCTAL_ESCAPE
    ;

fragment
UNICODE_ESCAPE
    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

fragment
OCTAL_ESCAPE
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
HEX_DIGIT : ('0'..'9'|'a'..'f'|'A'..'F')
          ;

COMMENT
    :   '/*' .*? '*/'    -> channel(HIDDEN) // match anything between /* and */
    ;

LINE_COMMENT
    : '//' ~[\r\n]* '\r'? ('\n' | EOF) -> channel(HIDDEN)
    ;

WS  :   [ \t\r\n]+ -> skip 
    ; // Define whitespace rule, toss it out
