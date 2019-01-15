all: parser
.PHONY: parser

CXX = g++
LEX = lex
YACC = yacc

parser: lex.yy.c y.tab.c
	$(CXX) -o $@ $^ -ly -lfl -std=c++14 -O2

lex.yy.c: lex.l
	$(LEX) lex.l

y.tab.c: yacc.y
	$(YACC) -d -v yacc.y

clean:
	rm -f parser lex.yy.c y.tab.c y.tab.h y.output

test: parser
