lexer grammar AntlrGrinderLexer;

options {
    language=Java;

}

@header {

    package com.sri.ai.grinder.parser.antlr;
    
}

@members {
@Override
public void recover(RecognitionException e) {
    throw new RuntimeException(e);
}
}

/*
    The lexer tokenizes the input string that the parser is asked to
    parse.  The tokens are all typed.  The supported types are ID, STRING,
    and the various constant types for terminal symbols.  Whitespace
    symbols will be excluded from the resulting token stream.

    Adding new grammar rules
    ------------------------
    Add any terminal symbols in the new grammar rules to the list below.
    
    Note: Ensure you update the corresponding list in AntlrGrinderTerminalSymbols.java
          with any changes made.
    
*/
PREVIOUS                : 'previous' ;
MESSAGE                 : 'message' ;
LAMBDA                  : 'lambda' ;
IF                      : 'if' ;
THEN                    : 'then' ;
ELSE                    : 'else' ;
THERE                   : 'there' ;
FOR                     : 'for' ;
ALL                     : 'all' ;
EXISTS                  : 'exists' ;
COLON                   : ':' ;
DOUBLE_ARROW            : '<=>' ;
ARROW                   : '=>' ;
SINGLE_ARROW            : '->' ;
NOT_EQUAL               : '!=' ;
GREATER_THAN_EQUAL      : '>=' ;
GREATER_THAN            : '>' ;
LESS_THAN_EQUAL         : '<=' ;
LESS_THAN               : '<' ;
EQUAL                   : '=' ;
PLUS                    : '+' ;
DASH                    : '-' ;
MINUS                   : 'minus' ;
TIMES                   : '*' ;
DIVIDE                  : '/' ;
CARAT                   : '^' ;
NOT                     : 'not' ;
IS                      : 'is' ;
CASE                    : 'case' ;
INDEX                   : 'index' ;
OCCURS                  : 'occurs' ;
FROM                    : 'from' ;
VARIABLE                : 'variable' ;
FACTOR                  : 'factor' ;
NEIGHBORS               : 'neighbors' ;
VALUE                   : 'value' ;
INTERSECTION            : 'intersection' ;
UNION                   : 'union' ;
OR                      : 'or' ;
AND                     : 'and' ;
IN                      : 'in' ;
OF                      : 'of' ;
ON                      : 'on' ;
TO                      : 'to' ;
CLOSE_SQUARE            : ']' ;
OPEN_SQUARE             : '[' ;
CLOSE_DOUBLE_CURLY      : '}}' ;
OPEN_DOUBLE_CURLY       : '{{' ;
UNDERSCORE_OPEN_CURLY   : '_{' ;
CLOSE_CURLY             : '}' ;
OPEN_CURLY              : '{' ;
VERT_BAR                : '|' ;
OPEN_PAREN              : '(' ;
CLOSE_PAREN             : ')' ;
COMMA                   : ',' ;
UNDERSCORE              : '_' ;
PERIOD                  : '.' ;



//SYMBOL
//    :   (ALPHANUMERIC | UNDERSCORE) (ALPHANUMERIC | UNDERSCORE)* ('\'')* |
//        '\'' (ALPHANUMERIC | UNDERSCORE) (ALPHANUMERIC | UNDERSCORE)* '\''+
//	;


STRING
    @init { final StringBuilder buf = new StringBuilder(); }
    : '\'' ( ESCAPE[buf] | i = ~( '\\' | '\'' ) { buf.appendCodePoint(i); } )* '\''
    { setText(buf.toString()); }
    |   '"' ( ESCAPE[buf] | i = ~( '\\' | '"' ) { buf.appendCodePoint(i); } )* '"'
    { setText(buf.toString()); }
    ;

ID
    : ALPHANUMERIC+ ('\'')*
    | FLOAT
//    | INT
//    | '-' NUMERIC+
    ;

//fragment
//INT :   '-'? NUMERIC+
//    ;

fragment 
FLOAT
    :   NUMERIC+ '.' NUMERIC* EXPONENT?
    |   '.' NUMERIC+ EXPONENT?
    |   NUMERIC+ EXPONENT
    ;


COMMENT
    :   '//' ~('\n'|'\r')* '\r'? '\n'? {$channel=HIDDEN;}
    |   '/*' ( options {greedy=false;} : . )* '*/' {$channel=HIDDEN;}
    ;



//CHAR:  '\'' ( ESC_SEQ | ~('\''|'\\') ) '\''
//    ;

//STRING
//    :  '\'' ( ESC_SEQ | ~('\\'|'\'') )* '\''
//    ;

fragment 
ESCAPE[StringBuilder buf] :
    '\\'
    ( 't' { buf.append('\t'); }
    | 'n' { buf.append('\n'); }
    | 'r' { buf.append('\r'); }
    | '\'' { buf.append('\''); }
    | '"' { buf.append('"'); }
    | '\\' { buf.append('\\'); }
    | 'u' a = HEX_DIGIT b = HEX_DIGIT c = HEX_DIGIT d = HEX_DIGIT { String string = new StringBuilder().append($a.text).append($b.text).append($c.text).append($d.text).toString();
                                                                    buf.append((char)(Integer.valueOf(string, 16).intValue())); }
    )
    ;

WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) {skip();}
    ;


fragment
EXPONENT : ('e'|'E') ('+'|'-')? NUMERIC+ ;


fragment
HEX_DIGIT : ( NUMERIC | 'a'..'f' | 'A'..'F' ) ;

//fragment
//ESC_SEQ
//    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
//    |   UNICODE_ESC
//    |   OCTAL_ESC
//    ;

//fragment
//OCTAL_ESC
//    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
//    |   '\\' ('0'..'7') ('0'..'7')
//    |   '\\' ('0'..'7')
//    ;

//fragment
//UNICODE_ESC
//    :   '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
//    ;


fragment
UPPER_CASE : ('A'..'Z') ;

fragment
LOWER_CASE : ('a'..'z') ;

fragment
ALPHA : (UPPER_CASE | LOWER_CASE) ;

fragment
NUMERIC : ('0'..'9') ;

fragment
ALPHANUMERIC : ( ALPHA | NUMERIC ) ;

