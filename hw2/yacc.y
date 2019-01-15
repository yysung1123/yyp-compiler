%{
#include <stdio.h>
#include <stdlib.h>

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
int yylex();
int yyerror(char *msg);
%}

%token SEMICOLON END IDENT VAR COLON INTEGER REAL STRING BOOLEAN ARRAY TO OF COMMA INTEGER_LITERAL REAL_LITERAL STRING_LITERAL TRUE FALSE LEFT_PARENTHESES RIGHT_PARENTHESES BGN ASSIGNMENT PRINT READ LEFT_BRACKET RIGHT_BRACKET ADD MINUS MUL DIV MOD LT LE EQU GE GT NE NOT AND OR IF THEN ELSE WHILE FOR DO RETURN
%left OR
%left AND
%left NOT
%nonassoc LT LE EQU GE GT NE
%left ADD MINUS 
%left MUL DIV MOD
%left unary

%%

program		: programname SEMICOLON programbody END IDENT
		;

programname	: identifier
		;

identifier	: IDENT
		;

programbody     : variable_declarations function_declarations compound_statement
                ;

variable_declarations   : variable_declaration variable_declarations
                        |
                        ;

function_declarations   : function_declaration function_declarations
                        |
                        ;

variable_declaration    : VAR identifier_list COLON type SEMICOLON
                        | VAR identifier_list COLON literal_constant SEMICOLON
                        ;

identifier_list : identifier next_identifier
                ;

next_identifier : COMMA identifier next_identifier
                |
                ;

type            : scalar_type
                | array
                ;

scalar_type     : INTEGER
                | REAL
                | STRING
                | BOOLEAN
                ;

array           : ARRAY INTEGER_LITERAL TO INTEGER_LITERAL OF type
                ;

literal_constant        : INTEGER_LITERAL
                        | REAL_LITERAL
                        | STRING_LITERAL
                        | TRUE
                        | FALSE
                        ;

function_declaration    : identifier LEFT_PARENTHESES argument_list RIGHT_PARENTHESES return_type SEMICOLON compound_statement END identifier
                        ;

argument_list   : identifier_list COLON type next_argument_list
                |
                ;

next_argument_list      : SEMICOLON argument_list
                        |
                        ;

return_type     : COLON type
                |
                ;

compound_statement      : BGN variable_declarations statements END
                        ;

statements      : statement statements
                |
                ;

statement       : compound_statement
                | simple_statement
                | conditional_statement
                | while_statement
                | for_statement
                | return_statement
                | function_invocation
                ;

simple_statement        : variable_reference ASSIGNMENT expression SEMICOLON 
                        | PRINT expression SEMICOLON
                        | READ variable_reference SEMICOLON
                        ;

variable_reference      : identifier
                        | array_reference
                        ;

array_reference : identifier index
                ;

index           : LEFT_BRACKET expression RIGHT_BRACKET next_index 
                ;

next_index      : index
                |
                ;

expression      : LEFT_PARENTHESES expression RIGHT_PARENTHESES 
                | MINUS expression %prec unary
                | expression ADD expression
                | expression MINUS expression
                | expression MUL expression
                | expression DIV expression
                | expression MOD expression
                | expression LT expression
                | expression LE expression
                | expression EQU expression
                | expression GE expression
                | expression GT expression
                | expression NE expression
                | NOT expression
                | expression AND expression
                | expression OR expression
                | variable_reference
                | procedure_call
                | literal_constant
                ;

conditional_statement   : IF expression THEN statements ELSE statements END IF
                        | IF expression THEN statements END IF
                        ;

while_statement : WHILE expression DO statements END DO
                ;

for_statement   : FOR identifier ASSIGNMENT INTEGER_LITERAL TO INTEGER_LITERAL DO statements END DO
                ;

return_statement        : RETURN expression SEMICOLON
                        ;

function_invocation     : procedure_call SEMICOLON
                        ;

procedure_call  : identifier LEFT_PARENTHESES parameters RIGHT_PARENTHESES
                ;

parameters      : expression next_expression
                |
                ;

next_expression : COMMA expression next_expression
                |
                ;

%%

int yyerror( char *msg )
{
        fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
	fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
	fprintf( stderr, "|\n" );
	fprintf( stderr, "| Unmatched token: %s\n", yytext );
        fprintf( stderr, "|--------------------------------------------------------------------------\n" );
        exit(-1);
}

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	exit(0);
}
