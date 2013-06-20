grammar AntlrGrinder;

expression : expr ;

expr : 
     // e.g.:(1+2)
     '(' expr ')' #parenthesesAroundExpression
     // function application, e.g.: f(X)
     | expr '(' ( expr (',' expr)* )? ')' # functionApplication
     // tuple, e.g.: (A, B, C)
     | '(' expr ',' expr (',' expr)* ')' #tuple
       // cardinality, e.g.: | X |
     | '|' expr '|' #cardinality
       // intensional multiset, e.g.: {{ (on X) f(X) | X != a }}
     | '{{' ('(' ON ( expr (',' expr)* )? ')')? expr ('|' expr)? '}}' #intensionalMultiset
       // extensional multiset, e.g.: {{ A, B, C, C, D }}
     | '{{' ( expr (',' expr)* )? '}}' #extensionalUniset
       // intensional uniset, e.g.: {{ (on X) f(X) | X != a }}
     | '{' ('{' ON ( expr (',' expr)* )? ')')? expr ('|' expr)? '}' #intensionalUniset
       // extensional uniset, e.g.: {{ A, B, C, C, D }}
     | '{' ( expr (',' expr)* )? '}' #extensionalUniset
       // bracketed expression, for parfactors and random variables, e.g. [if p(X) then 1 else 2]
     | '[' expr ']' #bracketedExpression
       // TODO?
     | VALUE OF expr #valueOf
       // TODO?
     | expr '_{' expr ':' expr '}' #underscoreCurly
       // TODO?
     | expr OCCURS IN expr #occursIn
       // TODO?
     | INDEX OF expr IN expr #indexOfIn
       // TODO?
     | CASE expr ( expr ':' expr (',' expr ':' expr)* )? #case
       // not, e.g.: not A and B -> (not(A)) and B
     | NOT expr #not
       // negative, e.g.: 2 * -1 -> 2 * (-1)
     | '-' expr #negative
       // power, e.g. 2^3^4 -> 2^(3^4)
     | expr '^'<assoc=right> expr #power
       // e.g.: 2*3/2 -> 2*(3/2)
     | expr '/' expr #division
       // e.g.: 1+2*3 -> 1+(2*3)
     | expr '*' expr #times
       // e.g.: 1-2+3 -> 1-(2+3)
     | expr '+' expr #plus
       // e.g.: 1-2
     | expr '-' expr #subtract
       // TODO ?
     | expr INTERSECTION expr #intersection
       // TODO ?
     | expr UNION expr #union
       // TODO ?
     | expr IN expr #in
       // comparison operators, e.g.: X = Y, 2 < 3
     | expr COMPARISON_OPERATOR expr #comparison
       // TODO ?
     | expr IS expr #is
       // e.g.: A or B and C -> A or (B and C)
     | expr AND expr #and
       // e.g.: A => B or C -> A => (B or C)
     | expr OR expr #or
       // e.g.: for all X : X != a
     | FOR ALL expr ':' expr #forAll
       // e.g.: there exists X : X = a
     | THERE EXISTS expr ':' expr #thereExists
       // e.g.: A = B => C = D
     | expr IMPLICATION expr #implication
       // e.g.: A = B <=> C = D
     | expr BICONDITIONAL expr #biconditional
       // e.g.: if X = Y then 1 else 2
     | IF expr THEN expr ELSE expr #ifThenElse
       // e.g.: lambda f(X) : 2 + f(X)
     | LAMBDA ( expr (',' expr)* )? ':' expr #lamda
       // TODO what is this meant to be?
     | expr SINGLE_ARROW expr #rightArrow
       // e.g.: previous message to <<expression>> from <<expression>>
     | PREVIOUS MESSAGE TO expr FROM expr #previousMessageToFrom
       // e.g.: message to <<expression>> from <<expression>>
     | MESSAGE TO expr FROM expr #messageToFrom
       // e.g.: neighbors of <<expression>> from <<expression>>
     | NEIGHBORS OF expr FROM expr #neighborsOfFrom
       // a symbol
     | SYMBOL #symbol
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
CASE                    : 'case' ;
ON                      : 'on' ;
IN                      : 'in' ;
VALUE                   : 'value' ;
OF                      : 'of' ;
INDEX                   : 'index' ;
OCCURS                  : 'occurs' ;
IS                      : 'is' ;
MINUS                   : 'minus' ;
PREVIOUS                : 'previous' ;
MESSAGE                 : 'message' ;
NEIGHBORS               : 'neighbors' ;
TO                      : 'to' ;
FROM                    : 'from' ;
FUNCTOR_TUPLE           : '( . )' ;
// Logic Operators
IMPLICATION             : '=>' ;
BICONDITIONAL           : '<=>' ;
// Arithmetic
POWER                   : '^' ;
DIVIDE                  : '/' ;
TIMES                   : '*' ;
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
COLON                   : ':' ;
SINGLE_ARROW            : '->' ;  // TODO - usage?
UNDERSCORE_OPEN_CURLY   : '_{' ;  // TODO - usage?
VERT_BAR                : '|' ;
COMMA                   : ',' ;
UNDERSCORE              : '_' ;
PERIOD                  : '.' ;

COMPARISON_OPERATOR
    : LESS_THAN 
    | LESS_THAN_EQUAL 
    | EQUAL 
    | NOT_EQUAL 
    | GREATER_THAN_EQUAL
    | GREATER_THAN
    ;

SYMBOL
    : RATIONAL
    | SYMBOLIC_NAME
    ;

RATIONAL
    : ('0' | '1'..'9' '0'..'9'*)
    | ('0'..'9')+ '.' ('0'..'9')* EXPONENT? FLOAT_TYPE_SUFFIX?
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

SYMBOLIC_NAME
    : ([a-zA-Z] | [0-9] | '_') ([a-zA-Z] | [0-9] | '_')* ('\'')*
    | ('^' '/' '*' '+' '-' '<' '!' '=' '>')+
    | '"'  (ESCAPE_SEQUENCE | ~('\\' | '"' ) )* '"'
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
    : '//' ~[\r\n]* '\r'? '\n' -> channel(HIDDEN)
    ;

WS  :   [ \t\r\n]+ -> skip 
    ; // Define whitespace rule, toss it out
