all: scanner

CC = gcc
LEX = lex

scanner: lex.yy.c
	$(CC) -o scanner lex.yy.c -lfl

lex.yy.c: hw1.l
	$(LEX) hw1.l

clean:
	rm scanner lex.yy.c

test: scanner
	./scanner test.p
