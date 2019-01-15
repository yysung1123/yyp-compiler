%{
#include "main.h"
#include "y.tab.h"
#define LIST               strcat(buf, yytext)
#define token(t)           {LIST; if (Opt_T) printf("<%s>\n", #t);}
#define tokenChar(t)       {LIST; if (Opt_T) printf("<%c>\n", (t));}
#define tokenInteger(t, i) {LIST; if (Opt_T) printf("<%s: %d>\n", #t, (i));}
#define tokenString(t, s)  {LIST; if (Opt_T) printf("<%s: %s>\n", #t, (s));}
#define tokenKeyword(t)    {LIST; if (Opt_T) printf("<KW%s>\n", (t));}
#define tokenStr(t)        {LIST; if (Opt_T) printf("<%s>\n", (t));}
#define MAX_LINE_LENG      256

int Opt_S = 1;
int Opt_T = 1;
extern int Opt_D;
int linenum = 1;
char buf[MAX_LINE_LENG];
char string_buf[MAX_LINE_LENG];
%}

digit   [0-9]
letter  [a-zA-Z]
identifier  {letter}({letter}|{digit})*
integer (0|[1-9]{digit}*)
octal_integer   0[0-7]+
float   0*{integer}\.{digit}+
scientific  (0*{integer}|{float})[eE][+-]?0*{integer}
string \"([^"\n]*\"{2})*[^"\n]*\"
space   [ \t]+

%x CCOMMENT

%%
  /* c-style comments */
"/*"    { LIST; BEGIN CCOMMENT; }
<CCOMMENT>.     { LIST; }
<CCOMMENT>"*/"  { LIST; BEGIN INITIAL; }
  /* c++-style comments */
"//&S+".*   { LIST; Opt_S = 1; }
"//&S-".*   { LIST; Opt_S = 0; }
"//&T+".*   { LIST; Opt_T = 1; }
"//&T-".*   { LIST; Opt_T = 0; }
"//&D+".*   { LIST; Opt_D = 1; }
"//&D-".*   { LIST; Opt_D = 0; }
"//".*  { LIST; }
  /* delimiters */
","	{ tokenChar(','); return COMMA; }
";"	{ tokenChar(';'); return SEMICOLON; }
":"	{ tokenChar(':'); return COLON; }
")"	{ tokenChar(')'); return RIGHT_PARENTHESES; }
"("	{ tokenChar('('); return LEFT_PARENTHESES; }
"["	{ tokenChar('['); return LEFT_BRACKET; }
"]"     { tokenChar(']'); return RIGHT_BRACKET; }
  /* operations */
"+"	{ tokenChar('+'); return ADD; }
"-"	{ tokenChar('-'); return MINUS; }
"*"	{ tokenChar('*'); return MUL; }
"/"	{ tokenChar('/'); return DIV; }
"mod"	{ token(mod); return MOD; }
":="	{ token(:=); return ASSIGNMENT; }
"<"	{ tokenChar('<'); return LT; }
"<="	{ token(<=); return LE; }
"<>"	{ token(<>); return NE; }
">="	{ token(>=); return GE; }
">"	{ tokenChar('>'); return GT; }
"="	{ tokenChar('='); return EQU; }
"and"	{ token(and); return AND; }
"or"	{ token(or); return OR; }
"not"	{ token(not); return NOT; }
  /* keywords */
"array"	    { token(KWarray); return ARRAY; }
"begin"	    { token(KWbegin); return BGN; }
"boolean"   {
              token(KWboolean);
              yylval.type.type = BOOL_T;
              return BOOLEAN;
            }
"def"	    { token(KWdef); }
"do"	    { token(KWdo); return DO; }
"else"	    { token(KWelse); return ELSE; }
"end"	    { token(KWend); return END; }
"false"	    {
              token(KWfalse);
              yylval.type.type = BOOL_T;
              yylval.attr.bval = false;
              return FALSE;
            }
"for"	    { token(KWfor); return FOR; }
"integer"   {
              token(KWinteger);
              yylval.type.type = INT_T;
              return INTEGER;
            }
"if"	    { token(KWif); return IF; }
"of"	    { token(KWof); return OF; }
"print"	    { token(KWprint); return PRINT; }
"read"	    { token(KWread); return READ; }
"real"	    {
              token(KWreal);
              yylval.type.type = REAL_T;
              return REAL;
            }
"string"    {
              token(KWstring);
              yylval.type.type = STR_T;
              return STRING;
            }
"then"	    { token(KWthen); return THEN; }
"to"	    { token(KWto); return TO; }
"true"	    {
              token(KWtrue);
              yylval.type.type = BOOL_T;
              yylval.attr.bval = true;
              return TRUE;
            }
"return"    { token(KWreturn); return RETURN; }
"var"	    { token(KWvar); return VAR; }
"while"	    { token(KWwhile); return WHILE; }
  /* identifier */
{identifier}    {
                  tokenString(id, yytext)
                  yylval.name = string(yytext);
                  return IDENT;
                }
  /* integer constants */
{integer}   {
              tokenString(integer, yytext) 
              yylval.type.type = INT_T;
              yylval.attr.ival = strtol(yytext, NULL, 10);
              return INTEGER_LITERAL; 
            }
{octal_integer} {
                  tokenString(oct_integer, yytext) 
                  yylval.type.type = INT_T;
                  yylval.attr.ival = strtol(yytext, NULL, 8);
                  return INTEGER_LITERAL; 
                }
  /* floating-point constants */
{float} {
          tokenString(float, yytext) 
          yylval.type.type = REAL_T;
          yylval.attr.rval = atof(yytext);
          return REAL_LITERAL;
         }
  /* scientific notations */
{scientific}    {
                  tokenString(scientific, yytext)
                  yylval.type.type = REAL_T;
                  yylval.attr.rval = atof(yytext);
                  return REAL_LITERAL;
                }
  /* string constants */
{string}    {
              int len = 0;
              for(int i = 1; i < yyleng - 1; ++i){
                string_buf[len++] = yytext[i];
                if(yytext[i] == '\"' && yytext[i + 1] == '\"') ++i;
              }
              string_buf[len] = '\0';
              tokenString(string, string_buf);
              yylval.type.type = STR_T;
              yylval.attr.sval = string(string_buf);
              return STRING_LITERAL;
            }
  /* whitespaces */
{space} { LIST; }
  /* invalid input */
.       {
          printf("error at line %d: bad character \"%s\"\n",linenum,yytext);
          exit(0);
        }
<INITIAL,CCOMMENT>\n    {
          //LIST;
          if (Opt_S)
            printf("%d: %s\n", linenum, buf);
          linenum++;
          buf[0] = '\0';
        }
%%
/*
int main( int argc, char **argv )
{
  if( argc != 2 ) {
    fprintf( stderr, "Usage: ./scanner [filename]\n" );
    exit(0);
  }
	
  FILE *fp = fopen( argv[1], "r" );
  if( fp == NULL ) {
    fprintf( stderr, "Open file error\n" );
    exit(-1);
  }
	
  yyin = fp;

  yylex();

  exit(0);
}
*/
int yywrap()
{
  return 1;
}
