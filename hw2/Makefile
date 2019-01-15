all: parser

CC = gcc
LEX = lex
YACC = yacc

parser: lex.yy.c y.tab.c
	$(CC) -o $@ $^ -ly -lfl

lex.yy.c: lex.l
	$(LEX) lex.l

y.tab.c: yacc.y
	$(YACC) -d -v yacc.y

clean:
	rm -f parser lex.yy.c y.tab.c y.tab.h y.output

test: parser
