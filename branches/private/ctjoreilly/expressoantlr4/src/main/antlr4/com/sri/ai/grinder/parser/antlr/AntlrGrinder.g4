grammar AntlrGrinder;

expression : expr ;

expr : 
     // e.g.:(1+2)
     '(' expr ')' #parenthesesAroundExpression
     // function application, e.g.: f(X)
     | functor=expr '(' ( args+=expr (',' args+=expr)* )? ')' # functionApplication
     // tuple, e.g.: (A, B, C)
     | '(' expr ',' expr (',' expr)* ')' #tuple
       // cardinality, e.g.: | X |
     | '|' expr '|' #cardinality
       // extensional multiset, e.g.: {{ A, B, C, C, D }}
     | '{{' ( expr (',' expr)* )? '}}' #extensionalMultiset
       // intensional multiset, e.g.: {{ (on X) f(X) | X != a }}
     | '{{' ('(' ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr ('|' condition=expr)? '}}' #intensionalMultiset
       // extensional uniset, e.g.: { A, B, C, C, D }
     | '{' ( expr (',' expr)* )? '}' #extensionalUniset
       // intensional uniset, e.g.: { (on X) f(X) | X != a }
     | '{' ('(' ON ( scopeargs+=expr (',' scopeargs+=expr)* )? ')')? head=expr ('|' condition=expr)? '}' #intensionalUniset
       // bracketed expression, for parfactors and random variables, e.g. [if p(X) then 1 else 2]
     | '[' expr ']' #bracketedExpression
       // value of, e.g.: value of(1 + 2)
     | VALUE OF expr #valueOf
       // underscore set, e.g.: x_{ y + 0.3 : { a, b, c } }
     | head=expr '_{' left=expr ':' right=expr '}' #underscoreCurly
       // occurs in, e.g.: x occurs in y
     | element=expr OCCURS IN collection=expr #occursIn
       // index of in, e.g.: index of x in y
     | INDEX OF of=expr IN in=expr #indexOfIn
       // case statement, e.g.: case x y : z, a : b
     | CASE arg=expr ( caseconiditions+=expr ':' caseactions+=expr (',' caseconiditions+=expr ':' caseactions+=expr)* )? #caseStatement
       // not, e.g.: not A and B -> (not(A)) and B
     | NOT expr #not
       // negative, e.g.: 2 * -1 -> 2 * (-1)
     | '-' expr #negative
       // exponentiation, e.g. 2^3^4 -> 2^(3^4)
     | base=expr '^'<assoc=right> exponent=expr #Exponentiation
       // division, e.g.: 2*3/2 -> 2*(3/2)
     | numerator=expr '/' denominator=expr #division
       // multiplication, e.g.: 1+2*3 -> 1+(2*3)
     | leftop=expr '*' rightop=expr #multiplication
       // addition, e.g.: 1-2+3 -> 1-(2+3)
     | leftop=expr '+' rightop=expr #addition
       // subtraction, e.g.: 1-2
     | minuend=expr '-' subtrahend=expr #subtraction
       // TODO ?
     | leftop=expr INTERSECTION rightop=expr #intersection
       // TODO ?
     | leftop=expr UNION rightop=expr #union
       // TODO ?
     | leftop=expr IN rightop=expr #in
       // comparison operators, e.g.: X = Y, 2 < 3
     | leftop=expr COMPARISON_OPERATOR rightop=expr #comparison
       // TODO ?
     | leftop=expr IS rightop=expr #is
       // e.g.: A or B and C -> A or (B and C)
     | leftconj=expr AND rightconj=expr #and
       // e.g.: A => B or C -> A => (B or C)
     | leftdisj=expr OR rightdisj=expr #or
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
     | (NOT | AND | OR | FOR | ALL | THERE | EXISTS
       | LAMBDA | IF | THEN | ELSE
       | INTERSECTION | UNION | CASE
       | ON | IN | VALUE | OF | INDEX | OCCURS
       | IS | MINUS | PREVIOUS | MESSAGE | NEIGHBORS | TO | FROM
       | IMPLICATION | BICONDITIONAL 
       | EXPONENTIATION | DIVIDE | TIMES | PLUS | SUBTRACT
       | LESS_THAN | LESS_THAN_EQUAL | EQUAL | NOT_EQUAL | GREATER_THAN_EQUAL | GREATER_THAN
       | COLON |SINGLE_ARROW | UNDERSCORE_OPEN_CURLY | VERT_BAR | UNDERSCORE | PERIOD
       | RATIONAL | SYMBOLIC_NAME) #symbol
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
// Logic Operators
IMPLICATION             : '=>' ;
BICONDITIONAL           : '<=>' ;
// Arithmetic
EXPONENTIATION          : '^' ;
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
    : '//' ~[\r\n]* '\r'? ('\n' | EOF) -> channel(HIDDEN)
    ;

WS  :   [ \t\r\n]+ -> skip 
    ; // Define whitespace rule, toss it out
